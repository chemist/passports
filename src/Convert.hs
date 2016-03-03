module Main where

import           Control.Applicative (liftA2, pure, (*>), (<$>), (<*), (<*>),
                                      (<|>))
import           System.Environment
import           Utils

main ::  IO ()
main = toPassportVector


