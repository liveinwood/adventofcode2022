-- stack script --resolver nightly-2022-11-08 --package containers --package bytestring --package vector --package mtl --package transformers

{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.State
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import           Debug.Trace

data Node = Dir String | File String Int deriving (Show, Eq, Ord)

type DirTree = M.Map [Node] (S.Set [Node])

data ParseState = ParseState {dirPath :: [Node], dirTree :: DirTree}

cd :: String -> State ParseState ()
cd name = do
    dirPath_ <- gets dirPath
    modify $ \ps -> ps {dirPath = Dir name : dirPath_}

upDir :: State ParseState ()
upDir = do
    dirPath_ <- tail <$> gets dirPath
    modify $ \ps -> ps {dirPath = dirPath_}

dir :: String -> State ParseState ()
dir name = do
    currentDir_ <- gets dirPath
    dirTree_ <- gets dirTree
    let dirTree' = if M.member currentDir_ dirTree_ then
                        M.adjust (S.insert (Dir name:currentDir_)) currentDir_ dirTree_
                   else
                        M.insert currentDir_ (S.singleton (Dir name:currentDir_)) dirTree_
    modify $ \ps -> ps {dirTree = dirTree'}

file :: String -> Int -> State ParseState ()
file name size = do
    currentDir_ <- gets dirPath
    dirTree_ <- gets dirTree
    let dirTree' = if M.member currentDir_ dirTree_ then
                        M.adjust (S.insert (File name size:currentDir_)) currentDir_ dirTree_
                   else
                        M.insert currentDir_ (S.singleton (File name size:currentDir_)) dirTree_
    modify $ \ps -> ps {dirTree = dirTree'}

mkDirTree :: String -> DirTree
mkDirTree s = do
    dirTree $ execState (sequence_ $ foldr helper [] $ lines s) $ ParseState {dirPath = [], dirTree = M.empty}
    where
        helper :: String -> [State ParseState ()] -> [State ParseState ()]
        helper s acc = case words s of
                        ["$", "cd", ".."] -> upDir:acc
                        ["$", "cd", dirname] -> cd dirname:acc
                        ["$", "ls"] -> acc
                        ["dir", dirname] -> dir dirname:acc
                        [size, fileName] -> file fileName (read size :: Int):acc
                        _ -> error "error"

dirSize :: DirTree -> [Node] -> Int
dirSize dirTree [] = error "error"
dirSize dirTree (File _ fileSize : _) = fileSize
dirSize dirTree dir = sum $ S.map (dirSize dirTree) (dirTree M.! dir)

solvePartOne = do
    dirTree <- mkDirTree <$> getContents
    return $ sum . filter (<= 100000) . map (dirSize dirTree) . M.keys $ dirTree

main = do
    n <- solvePartOne
    print n
