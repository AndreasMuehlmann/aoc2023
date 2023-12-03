import qualified Data.Text as T
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
    contents <- TIO.readFile "test_part1.txt"
    print contents
