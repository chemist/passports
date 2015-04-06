{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Char8 (pack)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import System.Environment


import Utils (check, input)

main ::  IO ()
main = main' =<< getArgs
  where
    main' [t] = case parseOnly input (pack t) of
                     Left e -> error e
                     Right p -> print =<< boolToInt `fmap` check p
    main' _ = error "passports_check NUM"
                     

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _    = 0
