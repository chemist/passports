{-# LANGUAGE OverloadedStrings #-}
module Web where

import           Control.Monad.IO.Class
import           Data.Text              (Text, pack)
import           Utils
import           Web.Spock.Safe

main :: IO ()
main = runSpock 3000 $ spockT id $ do
    get root $ file "index" "static/index.html"
    post "bulk" $ bulk

bulk :: ActionT IO ()
bulk = do
    passports <- body
    liftIO $ print passports
    text $ pack $ show passports

data Passport = Passport
  { passport    :: Text
  , checkResult :: Bool
  } deriving (Show, Eq)
