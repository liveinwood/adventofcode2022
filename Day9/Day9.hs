-- stack script --resolver nightly-2022-11-08 --package containers --package bytestring --package mtl --package transformers

{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad             (replicateM_)
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8     as BS
import qualified Data.Set                  as S
import           Debug.Trace

data Rope = Rope { rhead :: (Int, Int), rtail :: (Int, Int), tailset :: S.Set (Int, Int) } deriving (Show)

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
    where
        adjacent :: (Int, Int) -> (Int, Int) -> Bool
        adjacent h t = t `elem` neighbors h
        neighbors :: (Int, Int) -> [(Int, Int)]
        neighbors (x, y) = [(x + i, y + j) | i <- [-1, 0, 1], j <- [-1, 0, 1] ]

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

main = do
    solvePartOne
    -- putStrLn "OK"
