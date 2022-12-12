-- stack script --resolver nightly-2022-11-08 --package containers --package bytestring --package mtl --package transformers

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS

data Program = Noop | Addx Int

signale :: Int -> Int -> [Program] -> [Int] -> [(Int, Int)]-> [(Int, Int)]
signale cycle val [] _ acc = acc
signale cycle val _ [] acc = acc
signale cycle val (p:ps) (s:ss) acc =
    case p of
        Noop -> if cycle <= s && s <= cycle + 1 then
                    signale (cycle + 1) val ps ss ((s, val):acc)
                else
                    signale (cycle + 1) val ps (s:ss) acc
        Addx n -> if cycle <= s && s <= cycle + 2 then
                    signale (cycle + 2) (val + n) ps ss ((s, val):acc)
                  else
                    signale (cycle + 2) (val + n) ps (s:ss) acc

mkProgram :: IO [Program]
mkProgram = do
    map helper . BS.lines <$> BS.getContents
    where
        helper :: BS.ByteString -> Program
        helper s = case BS.words s of
                        ["noop"] -> Noop
                        ["addx", n] -> Addx $ read (BS.unpack n)
                        _ -> error "error"

-- solvePartOne :: [Program] -> [Int]
solvePartOne :: [Program] -> Int
solvePartOne programs = sum $ map (\(s, v) -> s * v) $ signale 0 1 programs [20, 60, 100, 140, 180, 220] []

main = do
    p <- mkProgram
    print $ solvePartOne p