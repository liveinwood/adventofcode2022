-- stack script --resolver nightly-2022-11-08 --package containers --package bytestring --package mtl --package transformers

{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
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
                        ["noop"]    -> Noop
                        ["addx", n] -> Addx $ read (BS.unpack n)
                        _           -> error "error"

solvePartOne :: [Program] -> Int
solvePartOne programs = sum $ map (\(s, v) -> s * v) $ signale 0 1 programs [20, 60, 100, 140, 180, 220] []


signalePart2 :: Int -> [Program] -> [Int]-> [Int]
signalePart2 val [] acc = acc
signalePart2 val (p:ps) acc =
    case p of
        Noop   -> signalePart2 val ps (val:acc)
        Addx n -> signalePart2 (val + n) ps (val:val:acc)

stripe :: Int -> String
stripe n = replicate (n - 1) '.' ++ "###" ++ replicate (40 - n - 1) '.'

crt :: Int ->  String
crt n = replicate (n - 1) '.' ++ "#" ++ replicate (40 - n + 1) '.'

divideSignal []      = []
divideSignal signale = take 40 signale : divideSignal (drop 40 signale)

draw :: [Int] -> [String]
draw signale = map (foldl helper2 (replicate 40 '.')) $ map (\sig -> zipWith helper (map crt [1..]) (map stripe sig)) (divideSignal signale)
    where
        helper :: String -> String -> String
        helper ss cc = zipWith helper' ss cc
        helper' '#' '#' = '#'
        helper' _ _     = '.'

        helper2 :: String -> String -> String
        helper2 ss cc = zipWith helper'' ss cc
        helper'' '#' _ = '#'
        helper'' _ '#' = '#'
        helper'' _ _   = '.'

main = do
    p <- mkProgram
    let signale = reverse $ signalePart2 1 p []
    let img = draw signale
    forM_ img $ \l -> do
        putStrLn l
