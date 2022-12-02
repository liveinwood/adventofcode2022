-- stack script --resolver nightly-2022-11-08 --package bytestring

import qualified Data.ByteString.Char8 as BS

-- A:Rock B:Paper C:Scissors
-- X:Rock Y:Paper Z:Scissors
data OpponentShape = A | B | C deriving (Read)
data MyShape =  X | Y | Z deriving (Read)

outComeScore :: (OpponentShape, MyShape) -> Int
outComeScore (A, Z) = 0
outComeScore (A, Y) = 6
outComeScore (B, X) = 0
outComeScore (B, Z) = 6
outComeScore (C, Y) = 0
outComeScore (C, X) = 6
outComeScore _      = 3

shapeScore :: MyShape -> Int
shapeScore X = 1
shapeScore Y = 2
shapeScore Z = 3

score :: (OpponentShape, MyShape) -> Int
score (os, ms) = outComeScore (os, ms) + shapeScore ms

partOneSolve :: IO ()
partOneSolve = do
    pairs <- map mkPair . BS.lines <$> BS.getContents :: IO [(OpponentShape, MyShape)]
    print $ foldl (\acc pair -> acc + score pair) 0 pairs

    where
        mkPair :: BS.ByteString -> (OpponentShape, MyShape)
        mkPair cs = (read . BS.unpack . BS.take 1 $ cs, read . BS.unpack . BS.take 1. BS.drop 2 $ cs)


--------------------- Part two

-- A:Rock B:Paper C:Scissors
-- X:lose Y:draw Z:win
myShape :: (OpponentShape, MyShape) -> MyShape
myShape (A, X) = Z
myShape (A, Y) = X
myShape (A, Z) = Y
myShape (B, X) = X
myShape (B, Y) = Y
myShape (B, Z) = Z
myShape (C, X) = Y
myShape (C, Y) = Z
myShape (C, Z) = X

partTwoSolve :: IO ()
partTwoSolve = do
    pairs <- map mkPair . BS.lines <$> BS.getContents :: IO [(OpponentShape, MyShape)]
    print $ foldl (\acc (os, ms) -> acc + score (os, myShape (os, ms))) 0 pairs

    where
        mkPair :: BS.ByteString -> (OpponentShape, MyShape)
        mkPair cs = (read . BS.unpack . BS.take 1 $ cs, read . BS.unpack . BS.take 1. BS.drop 2 $ cs)

main :: IO ()
main = partTwoSolve
