{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Text.Printf (printf)
import Data.Word (Word8, Word32)
import Control.Monad (forM, forM_, void)
import Control.Applicative ((<$>), (<*>), (<|>), (<*))
import Pipes (Producer, lift, (>->), runEffect, Consumer, await)
import System.IO (openFile, IOMode(..), hClose, Handle)
import Data.ByteString.Char8 (ByteString, hPut)
import Pipes.ByteString (fromHandle)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Data.Map (Map, fromList, (!))
import Data.Either 

import Data.Attoparsec.ByteString.Char8 (Parser, takeWhile, endOfLine, digit, decimal, char, endOfInput)
import qualified Pipes.Attoparsec as PA
import Data.Serialize (Serialize, put, get, runPut, putWord32host)
import qualified Data.Vector.Storable as V
import Data.Vector.Algorithms.Heap (sort)
import Data.Vector.Storable.MMap (unsafeMMapMVector, unsafeMMapVector, Mode(..))
import Control.Monad.Primitive (PrimState)
import Data.Bits (shiftR)
import Data.Char (digitToInt)

import Prelude hiding (elem, takeWhile)

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
toPassportVector f = do
    h <- openFile f ReadMode
    createDirectoryIfMissing True "passports" 
    vectors <- forM [0 .. 9] $ \x -> do
        v <- openFile ("passports" </> show x) ReadWriteMode
        return (x, v)
    void . runEffect $ PA.parsed parsePassport (fromHandle h) >-> write (fromList vectors)
    forM_ vectors $ \(_, v) -> do
        hClose v
    hClose h
    forM_ [0..9 :: Int] $ \x -> do
        sortMMapedVector ("passports" </> show x) 

instance Serialize Passport where
    put (Passport l b) = put l >> put b 
    get = Passport <$> get <*> get

write :: Map Word8 Handle -> Consumer (Either ByteString Passport) IO (Either (PA.ParsingError, Producer ByteString IO ()) ())
write vectors = do
    either (lift . print) write' =<< await
    write vectors
    where
      write' (Passport label' body) = lift $ hPut (getHandle label') (runPut $ putWord32host body)
      getHandle label' = vectors ! label'

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

