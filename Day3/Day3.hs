-- stack script --resolver nightly-2022-11-08 --package bytestring --package containers --package extra --package split
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import           Data.List.Split       (chunksOf)
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

solvePartTwo = do
    lines_ <- BS.lines <$> BS.getContents
    let commons = map (head . S.toList . intersection . map mkSet) $ chunksOf 3 lines_
    case getSum <$> foldMap (fmap Sum . (`M.lookup` priority)) commons of
        Just n  -> return n
        Nothing -> error "error"

    where
        mkSet :: BS.ByteString -> S.Set Char
        mkSet = S.fromList . BS.unpack
        intersection :: [S.Set Char] -> S.Set Char
        intersection (s:ss) = foldl S.intersection s ss


main = do
    n <- solvePartTwo
    print n
    -- n <- solvePartOne
    -- print n
