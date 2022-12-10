-- stack script --resolver nightly-2022-11-08 --package containers --package bytestring --package vector

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import qualified Data.List             as L
import qualified Data.Vector           as V

type Tree = (Int, Bool)

visibleFromL :: V.Vector Tree -> V.Vector Tree
visibleFromL ts = go 0 (V.length ts) (minBound :: Int) ts
    where
        go :: Int -> Int -> Int -> V.Vector Tree -> V.Vector Tree
        go i vlen highest tv = if i >= vlen then tv
                               else
                                    let
                                        height = fst $ tv V.! i
                                        visible = snd (tv V.! i) || height > highest
                                    in go (i + 1) vlen (max highest height) (tv V.// [(i, (height, visible))])

visibleFromR :: V.Vector Tree -> V.Vector Tree
visibleFromR = V.reverse . visibleFromL . V.reverse

visible :: V.Vector (V.Vector Tree) -> V.Vector (V.Vector Tree)
visible =  V.map (visibleFromR . visibleFromL) . transpose' . V.map (visibleFromR . visibleFromL)
    where
        transpose' :: V.Vector (V.Vector Tree) -> V.Vector (V.Vector Tree)
        transpose' = V.fromList . map V.fromList . L.transpose . V.toList . V.map V.toList

countVisible :: V.Vector (V.Vector Tree) -> Int
countVisible vtv = V.sum $ V.map helper vtv
    where
        helper :: V.Vector Tree -> Int
        helper = V.foldl (\acc (_, b) -> if b then acc + 1 else acc ) 0

toInts :: BS.ByteString -> [Int]
toInts s = go s []
    where
        go :: BS.ByteString -> [Int] -> [Int]
        go "" acc = reverse acc
        go s acc  = go (BS.tail s) ((read . BS.unpack . BS.take 1) s : acc)

make2dv :: BS.ByteString -> V.Vector (V.Vector Tree)
make2dv = V.fromList . map (V.fromList . (`zip` repeat False) . toInts) . BS.lines

solvePartOne = do
    vtv <- make2dv <$> BS.getContents
    return $ countVisible . visible $ vtv

main = do
    n <- solvePartOne
    print n
