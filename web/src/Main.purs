module Main where

import Prelude (Unit, (>>=), bind, return, ($), unit, (<$>))
import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM  (DOM)
import React (ReactElement, ReactClass, createFactory, createClass, spec, readState)
import ReactDOM (render)
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

main :: forall eff. Eff (ajax :: AJAX, dom :: DOM, console :: CONSOLE | eff) Unit
main = component myApp "hello"

component :: forall props eff. ReactClass props -> props -> Eff ( ajax :: AJAX, console :: CONSOLE, dom :: DOM | eff ) Unit
component app props = (container >>= render ui) *> return unit
  where
      ui :: ReactElement
      ui = D.div [ P.className "root" ]
                 [ createFactory app props ]

container :: forall eff. Eff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | eff) Element
container = do
  win <- window
  doc <- document win
  elm <- fromJust <$> toMaybe <$> body doc
  return $ htmlElementToElement elm

myApp :: ReactClass String
myApp = createClass (spec {} render)
  where
      render this = do
          st <- readState this
          return $ D.text "hello world"
