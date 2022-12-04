-- stack script --resolver nightly-2022-11-08 --package bytestring --package containers --package extra
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import qualified Data.Map              as M
import           Data.Monoid
import qualified Data.Set              as S
import qualified Data.Tuple.Extra      as E
import           Text.Read             (Lexeme (Char))

priority :: M.Map Char Int
priority = M.fromList $ zip ['a' .. 'z'] [1..] ++ zip ['A' .. 'Z'] [27..]

common :: BS.ByteString -> Char
common = head . S.toList . uncurry S.intersection . E.both mkSet . split
    where
        split :: BS.ByteString -> (BS.ByteString, BS.ByteString)
        split s = BS.splitAt (BS.length s `div` 2) s
        mkSet :: BS.ByteString -> S.Set Char
        mkSet = S.fromList . BS.unpack

solvePartOne :: IO Int
solvePartOne = do
    commons <- map common . BS.lines <$> BS.getContents
    case getSum <$> foldMap (fmap Sum . (`M.lookup` priority)) commons of
        Just n  -> return n
        Nothing -> error "error"

main = do
    n <- solvePartOne
    print n
