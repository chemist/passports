{-# LANGUAGE OverloadedStrings #-}
module Web where

import           Control.Applicative           (many)
import           Control.Monad.IO.Class
import           Data.Aeson                    hiding (json)
import           Data.Attoparsec.ByteString    (parseOnly)
import           Data.Text                     (Text, pack)
import           Network.HTTP.Types.Status     (status406)
import           Network.Wai.Middleware.Static
import           Utils                         hiding (Passport)
import           Web.Spock.Safe

main :: IO ()
main = runSpock 3100 $ spockT id $ do
    middleware (staticPolicy (addBase "static"))
    get root $ file "index" "static/index.html"
    post "bulk" $ bulk

bulk :: ActionT IO ()
bulk = do
    raw <- body
--    setHeader "Access-Control-Allow-Origin" "*"
    case parseOnly inputs raw of
         Left s -> do
             setStatus status406
             text (pack s)
         Right r -> json =<< (liftIO $ mapM (\x -> Passport (pack $ show x) <$> (check x)) r)

data Passport = Passport
  { passport :: Text
  , status   :: Bool
  } deriving (Show, Eq)

instance ToJSON Passport where
    toJSON (Passport passport' status') = object [ "passport" .= passport', "status" .= status' ]
