{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Text.Printf (printf)
import Data.Word (Word8, Word32)
import Control.Monad (forM, forM_, void)
import Control.Applicative ((*>), pure, (<$>), (<*>), liftA2, (<*), (<|>))
import Pipes (Producer, lift, (>->), runEffect, Consumer, await)
import System.IO (openFile, IOMode(..), hClose, Handle)
import Data.ByteString.Char8 (ByteString, hPut)
import qualified Pipes.Prelude as P
import Pipes.ByteString (fromHandle)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Data.Map (Map, fromList, (!))

import Data.Attoparsec.ByteString.Char8 (Parser, skipWhile, endOfLine, digit, decimal, char, endOfInput)
import qualified Pipes.Attoparsec as PA
import Data.Serialize (Serialize, put, get, runPut, putWord32host)
import qualified Data.Vector.Storable as V
import Data.Vector.Algorithms.Heap (sort)
import Data.Vector.Storable.MMap (unsafeMMapMVector, unsafeMMapVector, Mode(..))
import Control.Monad.Primitive (PrimState)
import Data.Bits (shiftR)

import Prelude hiding (elem)

input :: Parser Passport
input = Passport <$> label <*> decimal <* endOfInput
  where
    label = read . return <$> digit

data Passport = Passport Word8 Word32 

instance Show Passport where
    show (Passport label body) = printf "%d%09d" label body

badPassport :: Parser Passport
badPassport = skipWhile (/= '\n') *> endOfLine *> pure (Passport 100 0)

goodPassport :: Parser Passport
goodPassport = Passport <$> label <*> body <* endOfLine
  where
    label = read . return <$> digit
    body = liftA2 (+) 
                  ((* 1000000) <$> decimal <* char ',' )
                  decimal

toPassportVector :: FilePath -> IO ()
toPassportVector f = do
    h <- openFile f ReadMode
    createDirectoryIfMissing True "passports" 
    vectors <- forM [0 .. 9] $ \x -> do
        v <- openFile ("passports" </> show x) ReadWriteMode
        return (x, v)
    void . runEffect $ PA.parsed (goodPassport <|> badPassport) (fromHandle h) >-> P.filterM good >-> write (fromList vectors)
    forM_ vectors $ \(_, v) -> do
        hClose v
    hClose h
    forM_ [0..9 :: Int] $ \x -> do
        sortMMapedVector ("passports" </> show x) 
    where
      good (Passport 100 _) = print "error in file" >> return False
      good _ = return True

instance Serialize Passport where
    put (Passport l b) = put l >> put b 
    get = Passport <$> get <*> get

write :: Map Word8 Handle -> Consumer Passport IO (Either (PA.ParsingError, Producer ByteString IO ()) ())
write vectors = do
    Passport label body <- await
    lift $ hPut (getHandle label) (runPut $ putWord32host body)
    write vectors
    where
      getHandle label = vectors ! label

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
      where
        k = (l + u) `shiftR` 1

check :: Passport -> IO Bool
check (Passport label body) = do
    v <- unsafeMMapVector ("passports" </> show label) Nothing :: IO (V.Vector Word32)
    return $ elem v body

