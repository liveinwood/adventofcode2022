-- stack script --resolver nightly-2022-11-08 --package containers --package bytestring --package mtl --package transformers

{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad             (replicateM_)
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8     as BS
import qualified Data.Set                  as S
import           Debug.Trace

data Rope = Rope { rhead :: (Int, Int), rtail :: (Int, Int), tailset :: S.Set (Int, Int) } deriving (Show)

adjacent :: (Int, Int) -> (Int, Int) -> Bool
adjacent h t = t `elem` neighbors h

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + i, y + j) | i <- [-1, 0, 1], j <- [-1, 0, 1] ]

move :: (Int, Int) -> StateT Rope IO ()
move (dx, dy) = do
    (hx, hy) <- gets rhead
    (tx, ty) <- gets rtail
    tset <- gets tailset
    let (hx', hy') = (hx + dx, hy + dy)
    -- lift . print $ show (hx, hy) ++ " -> " ++ show (hx', hy')
    if adjacent (hx', hy') (tx, ty) then
        put $ Rope { rhead = (hx', hy'), rtail = (tx, ty), tailset = tset }
    else
        put $ Rope { rhead = (hx', hy'), rtail = (hx, hy), tailset = S.insert (hx, hy) tset }

-- up :: State Rope ()
up = move (0, -1)

-- down :: State Rope ()
down = move (0, 1)

-- right :: State Rope ()
right = move (1, 0)

-- left :: State Rope ()
left = move (-1, 0)

motionList :: BS.ByteString -> StateT Rope IO ()
motionList s = do
    mapM_ toMotion (BS.lines s)
    where
        toMotion :: BS.ByteString -> StateT Rope IO ()
        toMotion s = do
            case BS.words s of
                ["U", n] -> replicateM_ (read $ BS.unpack n) up
                ["D", n] -> replicateM_ (read $ BS.unpack n) down
                ["R", n] -> replicateM_ (read $ BS.unpack n) right
                ["L", n] -> replicateM_ (read $ BS.unpack n) left
                _        -> error "error"

solvePartOne = do
    s <- BS.getContents
    let ml = motionList s
    state <- execStateT ml $ Rope {rhead = (0, 0), rtail = (0, 0), tailset = S.fromList [(0, 0)]}
    print $ S.size . tailset $ state

----------------------------- Part 2 --------------------------

data Rope2 = Rope2 { rope :: [(Int, Int)], tailset2 :: S.Set (Int, Int) } deriving (Show)

move2 :: (Int, Int) -> StateT Rope2 IO ()
move2 (dx, dy) = do
    r <- gets rope
    s <- gets tailset2
    let r' = moveRope (dx, dy) r
    put $ Rope2 { rope = r', tailset2 = S.insert (r' !! 9) s }


moveRope :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
moveRope (dx, dy) ((x, y):rest) = moveTail [(x + dx, y + dy)] rest

moveTail moved [] = reverse moved
moveTail ((x, y):moved) ((x1, y1):rest) =
    if adjacent (x, y) (x1, y1) then reverse ((x, y):moved) ++ ((x1, y1):rest)
    else
        let (dx, dy) = direction (x, y) (x1, y1)
        in moveTail ((x1 + dx, y1 + dy):(x, y):moved) rest
    where
        direction :: (Int, Int) -> (Int, Int) -> (Int, Int)
        direction (hx, hy) (tx, ty) = (direct hx tx, direct hy ty)
        direct h t
          | h > t = 1
          | h == t = 0
          | otherwise = -1

motionList2 :: BS.ByteString -> StateT Rope2 IO ()
motionList2 s = do
    mapM_ toMotion (BS.lines s)
    where
        toMotion :: BS.ByteString -> StateT Rope2 IO ()
        toMotion s = do
            case BS.words s of
                ["U", n] -> replicateM_ (read $ BS.unpack n) (move2 (0, -1))
                ["D", n] -> replicateM_ (read $ BS.unpack n) (move2 (0, 1))
                ["R", n] -> replicateM_ (read $ BS.unpack n) (move2 (1, 0))
                ["L", n] -> replicateM_ (read $ BS.unpack n) (move2 (-1, 0))
                _        -> error "error"

solvePartTwo = do
    s <- BS.getContents
    let ml = motionList2 s
    state <- execStateT ml $ Rope2 {rope = replicate 10 (0, 0), tailset2 = S.empty }
    print $ S.size . tailset2 $ state

main = do
    solvePartTwo
