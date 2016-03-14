module Main where

import Prelude (Unit, (>>=), bind, return, ($), unit, (<$>), (<<<), show, void, class Show, (++), map)
import Data.Array (filter)
import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, print, log)
import DOM  (DOM)
import React (ReactElement, ReactClass, createFactory, createClass, spec, readState, render, transformState)
import React.DOM as D
import React.DOM.Props as P
-- import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import Network.HTTP.Affjax (AJAX, post)
import Control.Monad.Aff (runAff)

import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window (document)
import DOM.Node.Types (Element())
import Unsafe.Coerce
import Data.Foreign.Class (class IsForeign, readProp, readJSON)
import Data.Either (Either(..))

foreign import setValueById :: forall eff. String -> String -> Eff eff Unit

main :: forall eff. Eff (ajax :: AJAX, dom :: DOM, console :: CONSOLE | eff) Unit
main = component myApp unit

component :: forall props eff. ReactClass props -> props -> Eff ( ajax :: AJAX, console :: CONSOLE, dom :: DOM | eff ) Unit
component app props = (container >>= render ui) *> return unit
  where
      ui :: ReactElement
      ui = D.div [ P.className "root" ]
                 [ createFactory app props ]

type Hello = { hello :: String }

newtype Passport = Passport { passport :: String, status :: Boolean }

instance showPassport :: Show Passport where
    show (Passport s) = show s.passport ++ " " ++ show s.status

instance passportIsForeign :: IsForeign Passport where
    read value = do
        n <- readProp "passport" value
        b <- readProp "status" value
        return $ Passport { passport : n, status : b }

type Passports = { raw :: String, state :: Array Passport }

container :: forall eff. Eff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | eff) Element
container = do
  win <- window
  doc <- document win
  elm <- fromJust <$> toMaybe <$> body doc
  return $ htmlElementToElement elm

bulkUrl :: String
bulkUrl = "/bulk"

myApp :: ReactClass Unit
myApp = createClass (spec { raw : "", state : []} render)
  where
      render this = do
          st <- readState this
          return $ 
            D.div'
              [ D.p' [ D.text "Введите паспорта" ] 
              , D.div'
                [ D.button [ P.className "check"
                           , P.onClick $ \e -> do
                               state <- readState this 
                               runAff err (ok this) (post bulkUrl state.raw)
                           ]
                           [ D.text "Проверить" ]
                , D.button [ P.className "clean" 
                           , P.onClick $ \e -> do
                               transformState this $ \st -> { raw : "", state : [] }
                               setValueById "bulk-body" ""
                           ]
                           [ D.text "Очистить" ]
                ]
              , D.ul' (map (\x -> D.li' [ D.text (show x) ])  (filter (\(Passport p) -> p.status) st.state))
              , D.textarea [ P.className "bulk-body"
                           , P._id "bulk-body"
                           , P.cols "40"
                           , P.rows "3"
                           , P.onChange $ \e -> do
                               let text = (unsafeCoerce e).target.value
                               transformState this $ \st -> { raw : text, state : st.state }
                               print text
                           ] [ D.text "" ]
              ]
      err = log <<< show
      ok this r = void $ liftEff $ transformState this \st -> { raw : st.raw, state : (unpack r.response) }


unpack :: String -> Array Passport
unpack response = case readJSON response of
                       Left _ -> []
                       Right r -> (r :: Array Passport)
