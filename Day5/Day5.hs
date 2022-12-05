-- stack script --resolver nightly-2022-11-08 --package bytestring --package vector --package mtl --package transformers

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State       (execState)
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8     as BS
import qualified Data.Vector               as V

type Stacks = V.Vector [Char]

stacks :: Stacks
stacks = V.fromList [ ""
                    , "PVZWDT"
                    , "DJFVWSL"
                    , "HBTVSLMZ"
                    , "JSR"
                    , "WLMFGBZC"
                    , "BGRZHVWQ"
                    , "NDBCPJV"
                    , "QBTP"
                    , "CRZGH"
                    ]

move :: Int -> Int -> Int -> State Stacks ()
move n from to = replicateM_ n (helper from to)
    where
        helper :: Int -> Int -> State Stacks ()
        helper from to = do
            stacks <- get
            let
                fromStack = stacks V.! from
                toStack = stacks V.! to
                fromStack' = tail fromStack
                toStack' = head fromStack : toStack
                stacks' =  stacks V.// [(from, fromStack'), (to, toStack')]
            put stacks'

-- mkOperation :: IO (State Stacks ())
solve = do
        lines_ <- BS.lines <$> BS.getContents :: IO [BS.ByteString]
        let operation = mapM_ helper lines_
            s = execState operation stacks
        return $ V.map head (V.tail s)
    where
        helper :: BS.ByteString -> State Stacks ()
        helper s = let [_, n, _, from, _, to] = BS.words s
                   in move (read . BS.unpack $ n) (read . BS.unpack $ from) (read . BS.unpack $ to)

main = do
    s <- solve
    print s
