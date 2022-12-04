-- stack script --resolver nightly-2022-11-08 --package bytestring
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BS

newtype Range = Range (Int, Int) deriving (Show)

contains :: Range -> Range -> Bool
contains (Range (a1, b1)) (Range (a2, b2)) = a1 <= a2 && b1 >= b2

overlap  :: Range -> Range -> Bool
overlap (Range (a1, b1)) (Range (a2, b2)) = (a1 >= a2 && a1 <= b2) || (b1 >= a2 && b1 <= b2)

mkRangePair :: BS.ByteString -> Maybe (Range, Range)
mkRangePair s = do
    (a, s1) <- BS.readInt s
    (b, s2) <- BS.readInt . BS.drop 1 $ s1
    (c, s3) <- BS.readInt . BS.drop 1 $ s2
    (d, s3) <- BS.readInt . BS.drop 1 $ s3
    return (Range(a,b), Range(c,d))

solvePartOne :: IO Int
solvePartOne = do
    p <- map mkRangePair . BS.lines <$> BS.getContents
    return $ foldl helper 0 p
    where
        helper :: Int -> Maybe (Range, Range) -> Int
        helper acc (Just (r1, r2)) = if contains r1 r2 || contains r2 r1 then acc + 1 else acc
        helper acc Nothing = error "error"

solvePartTwo :: IO Int
solvePartTwo = do
    p <- map mkRangePair . BS.lines <$> BS.getContents
    return $ foldl helper 0 p
    where
        helper :: Int -> Maybe (Range, Range) -> Int
        helper acc (Just (r1, r2)) = if overlap r1 r2 || overlap r2 r1 then acc + 1 else acc
        helper acc Nothing = error "error"

main = do
    -- n <- solvePartOne
    n <- solvePartTwo
    print n