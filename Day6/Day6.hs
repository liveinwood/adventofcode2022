-- stack script --resolver nightly-2022-11-08 --package bytestring --package vector --package mtl --package transformers

{-# LANGUAGE OverloadedStrings #-}

-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Map as M
import qualified Data.List as L

solvePartOne :: String -> Int
solvePartOne = go ""
    where
        go _ "" = error "error"
        go acc (c:cs) | length acc <= 3 = go (c:acc) cs
                      | unique (c: take 3 acc) = length (c:acc)
                      | otherwise = go (c:acc) cs
            where
                unique cs = (length . L.nub) cs == length cs

solvePartTwo :: String -> Int
solvePartTwo = go ""
    where
        go _ "" = error "error"
        go acc (c:cs) | length acc <= 13 = go (c:acc) cs
                      | unique (c: take 13 acc) = length (c:acc)
                      | otherwise = go (c:acc) cs
            where
                unique cs = (length . L.nub) cs == length cs
main = do
    s <- getLine
    print $ solvePartTwo s
