-- stack script --resolver nightly-2022-11-08 --package bytestring --package extra
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.Extra       as LE

main = do
    ans <- partOneSolve
    print ans


partOneSolve :: IO Int
partOneSolve = maximum . map sum' . LE.split (== Nothing) . map (fmap fst . BS.readInt) . BS.lines <$> BS.getContents
    where
        sum' = foldl add 0
        sum' :: [Maybe Int] -> Int

        add :: Int -> Maybe Int -> Int
        add acc (Just n) = acc + n
        add acc Nothing  = acc
