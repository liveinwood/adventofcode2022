-- stack script --resolver nightly-2022-11-08 --package containers --package bytestring --package vector

{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString       (maximum)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List             as L
import qualified Data.Vector           as V

type Tree a = (Int, a)
type VTree = Tree Bool

visibleFromL :: V.Vector VTree -> V.Vector VTree
visibleFromL ts = go 0 (V.length ts) (minBound :: Int) ts
    where
        go :: Int -> Int -> Int -> V.Vector VTree -> V.Vector VTree
        go i vlen highest tv = if i >= vlen then tv
                               else
                                    let
                                        height = fst $ tv V.! i
                                        visible = snd (tv V.! i) || height > highest
                                    in go (i + 1) vlen (max highest height) (tv V.// [(i, (height, visible))])

visibleFromR :: V.Vector VTree -> V.Vector VTree
visibleFromR = V.reverse . visibleFromL . V.reverse

visible :: V.Vector (V.Vector VTree) -> V.Vector (V.Vector VTree)
visible =  V.map (visibleFromR . visibleFromL) . transpose' . V.map (visibleFromR . visibleFromL)
    where
        transpose' :: V.Vector (V.Vector VTree) -> V.Vector (V.Vector VTree)
        transpose' = V.fromList . map V.fromList . L.transpose . V.toList . V.map V.toList

countVisible :: V.Vector (V.Vector VTree) -> Int
countVisible vtv = V.sum $ V.map helper vtv
    where
        helper :: V.Vector VTree -> Int
        helper = V.foldl (\acc (_, b) -> if b then acc + 1 else acc ) 0

toInts :: BS.ByteString -> [Int]
toInts s = go s []
    where
        go :: BS.ByteString -> [Int] -> [Int]
        go "" acc = reverse acc
        go s acc  = go (BS.tail s) ((read . BS.unpack . BS.take 1) s : acc)

make2dv :: a -> BS.ByteString -> V.Vector (V.Vector (Tree a))
make2dv a = V.fromList . map (V.fromList . (`zip` repeat a) . toInts) . BS.lines

solvePartOne = do
    vtv <- make2dv False <$> BS.getContents
    return $ countVisible . visible $ vtv


---------------------------- Part 2 ---------------------------

type DTree = Tree [Int]

distanceL ::V.Vector DTree -> V.Vector DTree
distanceL vtv = go 0 (V.length vtv) vtv
    where
        go :: Int -> Int -> V.Vector DTree -> V.Vector DTree
        go i vlen v = if i < vlen then
                        let
                            (h, ds) = v V.! i
                            d = i - getDistance v h (i - 1)
                        in go (i + 1) vlen (v V.// [(i, (h, d:ds))])
                      else
                        v

        getDistance :: V.Vector DTree -> Int -> Int -> Int
        getDistance v h i
          | i <= 0 = 0
          | fst (v V.! i) >= h = i
          | otherwise = getDistance v h (i - 1)

distanceR ::V.Vector DTree -> V.Vector DTree
distanceR = V.reverse . distanceL . V.reverse

distance :: V.Vector (V.Vector DTree) -> V.Vector (V.Vector DTree)
distance =  V.map (distanceR . distanceL) . transpose' . V.map (distanceR . distanceL)
    where
        transpose' :: V.Vector (V.Vector DTree) -> V.Vector (V.Vector DTree)
        transpose' = V.fromList . map V.fromList . L.transpose . V.toList . V.map V.toList

solvePartTwo = do
    vtv <- make2dv [] <$> BS.getContents
    return $ V.maximum . V.map (V.maximum . V.map product') . distance $ vtv
    where
        product' :: DTree -> Int
        product' (_, ds) = product ds

main = do
    n <- solvePartTwo
    print n
