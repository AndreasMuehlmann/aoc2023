import qualified Data.ByteString.Char8 as B
import Data.Char
import System.IO

firstDigit :: B.ByteString -> Char
firstDigit line = B.head firstNotDigitsDropped
    where firstNotDigitsDropped = B.dropWhile (not . isDigit) line


lastDigit :: B.ByteString -> Char
lastDigit line = B.last lastNotDigitsDropped
    where lastNotDigitsDropped = B.dropWhileEnd (not . isDigit) line

lastFirstDigitAsNumber :: B.ByteString -> Integer
lastFirstDigitAsNumber line = read concatedDigits :: Integer
    where concatedDigits = [firstDigit line, lastDigit line]

main :: IO ()
main = do
    contents <- B.readFile "part1.txt"
    print $ sum $ map lastFirstDigitAsNumber (B.lines contents)
