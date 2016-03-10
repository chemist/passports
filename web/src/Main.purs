module Main where

import Prelude (Unit, (>>=), bind, return, ($), unit, (<$>))
import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)
import DOM  (DOM)
import React (ReactElement, ReactClass, createFactory, createClass, spec, readState, render, transformState)
import React.DOM as D
import React.DOM.Props as P
-- import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import Network.HTTP.Affjax (AJAX)

import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window (document)
import DOM.Node.Types (Element())
import Unsafe.Coerce

main :: forall eff. Eff (ajax :: AJAX, dom :: DOM, console :: CONSOLE | eff) Unit
main = component myApp unit

component :: forall props eff. ReactClass props -> props -> Eff ( ajax :: AJAX, console :: CONSOLE, dom :: DOM | eff ) Unit
component app props = (container >>= render ui) *> return unit
  where
      ui :: ReactElement
      ui = D.div [ P.className "root" ]
                 [ createFactory app props ]

type Hello = { hello :: String }

type Passport = { passport :: String, status :: Boolean }
type Passports = { raw :: String, state :: Array Passport }

container :: forall eff. Eff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | eff) Element
container = do
  win <- window
  doc <- document win
  elm <- fromJust <$> toMaybe <$> body doc
  return $ htmlElementToElement elm

myApp :: ReactClass Unit
myApp = createClass (spec { raw : "", state : []} render)
  where
      render this = do
          st <- readState this
          return $ 
            D.div'
              [ D.p' [ D.text "Введите паспорта" ] 
              , D.div'
                [ D.button [ P.className "check" ]
                           [ D.text "Проверить" ]
                , D.button [ P.className "clean" ]
                           [ D.text "Очистить" ]
                ]
              , D.ul' [ D.li' [ ]]
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
