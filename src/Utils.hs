{-# LANGUAGE OverloadedStrings #-}
module Utils where

import           Control.Applicative              (many, (<|>))
import           Control.Monad                    (forM_, void)
import           Control.Monad.IO.Class
import           Data.ByteString.Char8            (ByteString, hPut, readFile)
import           Data.Either
import           Data.Word                        (Word32, Word8)
import           Pipes                            (Consumer, Producer, await,
                                                   runEffect, (>->))
import           System.Directory                 (createDirectoryIfMissing)
import           System.FilePath                  ((</>))
import           System.IO                        (IOMode (..), withFile)
import           Text.Printf                      (printf)

import           Control.Monad.Morph
import           Control.Monad.Primitive          (PrimState)
import           Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, digit,
                                                   endOfInput, endOfLine, sepBy,
                                                   skipMany, skipMany1, space,
                                                   takeWhile)
import           Data.Bits                        (shiftR)
import           Data.Char                        (digitToInt)
import           Data.Serialize                   (Serialize, get,
                                                   getWord32host, put,
                                                   putWord32host, runGet,
                                                   runPut)
import           Data.Vector.Algorithms.Heap      (sort)
import qualified Data.Vector.Storable             as V
import           Data.Vector.Storable.MMap        (Mode (..), unsafeMMapMVector,
                                                   unsafeMMapVector)
import qualified Pipes.Attoparsec                 as PA
import           Pipes.BZip
import           Pipes.HTTP
import           Pipes.Safe

import           Prelude                          hiding (elem, map, null,
                                                   readFile, takeWhile)

input :: Parser Passport
input = Passport <$> label <*> decimal <* (endOfInput <|> endOfLine)

inputs :: Parser [Passport]
inputs = (passport `sepBy` sp) <* end
  where
    passport = Passport <$> label <*> decimal
    sp = skipMany space *> skipMany endOfLine <* skipMany space
    end = skipMany space *> skipMany endOfLine *> skipMany space *> endOfInput


label :: Parser Word8
label = fromIntegral . digitToInt <$> digit

data Passport = Passport Word8 Word32

instance Show Passport where
    show (Passport label' body) = printf "%d%09d" label' body

parsePassport :: Parser (Either ByteString Passport)
parsePassport = good <|> bad
    where
        bad = Left <$> takeWhile (/= '\n') <* endOfLine

        good = Right <$> (Passport <$> label <*> body <* endOfLine)
          where
            body = do
                x <- decimal <* char ','
                y <- decimal
                return $ x * 1000000 + y

toPassportVector :: IO ()
toPassportVector = do
    req <- parseUrl "http://www.fms.gov.ru/upload/expired-passports/list_of_expired_passports.csv.bz2"
    let req' = req { method = "GET" }
    createDirectoryIfMissing True "passports"
    void $ withManager defaultManagerSettings $ \m ->
        withHTTP req' m $ \resp ->
          runSafeT . runEffect $ PA.parsed parsePassport (bunzip2 (hoist liftIO $ responseBody resp)) >-> write
--           void . runEffect $ PA.parsed parsePassport (fromHandle h) >-> write
    forM_ [0..9 :: Int] $ \x -> do
        sortMMapedVector ("passports" </> show x)

instance Serialize Passport where
    put (Passport l b) = put l >> put b
    get = Passport <$> get <*> get

write :: (MonadSafe m) => Consumer (Either ByteString Passport) m (Either (PA.ParsingError, Producer ByteString m ()) ())
write = do
    either (liftIO . print) write' =<< await
    write
    where
      write' (Passport label' body) = liftIO $ withFile ("passports" </> show label') AppendMode (flip hPut (runPut $ putWord32host body))

recovery :: FilePath -> FilePath -> IO ()
recovery base result = forM_ [0..9::Int] $ \f -> do
    body <- readFile (base </> show f)
    let Right l = runGet getList body
    mapM_ (\x -> appendFile result $ (show (Passport (fromIntegral f) x)) ++ "\n") l
    where
    getList = many getWord32host

sortMMapedVector :: FilePath -> IO ()
sortMMapedVector fp = sort =<< (unsafeMMapMVector fp ReadWrite Nothing :: IO (V.MVector (PrimState IO) Word32))

elem :: (V.Storable a, Ord a) => V.Vector a -> a -> Bool
elem v a = loop 0 (V.length v)
    where
    loop l u
      | u <= l = False
      | a > v V.! k = loop (k + 1) u
      | a == v V.! k = True
      | a < v V.! k = loop l k
      | otherwise = error "BUG: elem"
      where
        k = (l + u) `shiftR` 1

check :: Passport -> IO Bool
check (Passport label' body) = do
    v <- unsafeMMapVector ("passports" </> show label') Nothing :: IO (V.Vector Word32)
    return $ elem v body

