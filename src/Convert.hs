module Main where

import Control.Applicative ((*>), pure, (<$>), (<*>), liftA2, (<*), (<|>))
import System.Environment
import Utils

main ::  IO ()
main = toPassportVector =<< head <$> getArgs


