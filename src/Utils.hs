{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Text.Printf (printf)
import Data.Word (Word8, Word32)
import Control.Monad (forM_, void)
import Control.Applicative ((<$>), (<*>), (<|>), (<*), many)
import Pipes (Producer, lift, (>->), runEffect, Consumer, await)
import System.IO (IOMode(..), withFile)
import Data.ByteString.Char8 (ByteString, hPut, readFile)
import Pipes.ByteString (fromHandle)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Data.Either 

import Data.Attoparsec.ByteString.Char8 (Parser, takeWhile, endOfLine, digit, decimal, char, endOfInput)
import qualified Pipes.Attoparsec as PA
import Data.Serialize (Serialize, put, get, runPut, putWord32host, runGet, getWord32host)
import qualified Data.Vector.Storable as V
import Data.Vector.Algorithms.Heap (sort)
import Data.Vector.Storable.MMap (unsafeMMapMVector, unsafeMMapVector, Mode(..))
import Control.Monad.Primitive (PrimState)
import Data.Bits (shiftR)
import Data.Char (digitToInt)

import Prelude hiding (elem, takeWhile, readFile)

input :: Parser Passport
input = Passport <$> label <*> decimal <* endOfInput

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


toPassportVector :: FilePath -> IO ()
toPassportVector f = withFile f ReadMode $ \h -> do
    createDirectoryIfMissing True "passports" 
    void . runEffect $ PA.parsed parsePassport (fromHandle h) >-> write 
    forM_ [0..9 :: Int] $ \x -> do
        sortMMapedVector ("passports" </> show x) 

instance Serialize Passport where
    put (Passport l b) = put l >> put b 
    get = Passport <$> get <*> get

write :: Consumer (Either ByteString Passport) IO (Either (PA.ParsingError, Producer ByteString IO ()) ())
write = do
    either (lift . print) write' =<< await
    write 
    where
      write' (Passport label' body) = lift $ withFile ("passports" </> show label') AppendMode (flip hPut (runPut $ putWord32host body))

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

