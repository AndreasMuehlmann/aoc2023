import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char
import System.IO

firstDigit :: T.Text -> Char
firstDigit line = T.head firstNotDigitsDropped
    where firstNotDigitsDropped = T.dropWhile (not . isDigit) line

textDigitsToDigits :: T.Text -> T.Text
textDigitsToDigits = T.replace (T.pack "one") (T.pack "1")
                     . T.replace (T.pack "two") (T.pack "2")
                     . T.replace (T.pack "three") (T.pack "3")
                     . T.replace (T.pack "four") (T.pack "4")
                     . T.replace (T.pack "five") (T.pack "5")
                     . T.replace (T.pack "six") (T.pack "6")
                     . T.replace (T.pack "seven") (T.pack "7")
                     . T.replace (T.pack "eight") (T.pack "8")
                     . T.replace (T.pack "nine") (T.pack "9")

lastDigit :: T.Text -> Char
lastDigit line = T.last lastNotDigitsDropped
    where lastNotDigitsDropped = T.dropWhileEnd (not . isDigit) line

lastFirstDigitAsNumber :: T.Text -> Integer
lastFirstDigitAsNumber line = read concatedDigits :: Integer
    where lineWithTextDigitsAsDigits = textDigitsToDigits line
          concatedDigits = [firstDigit lineWithTextDigitsAsDigits, lastDigit lineWithTextDigitsAsDigits]

main :: IO ()
main = do
    contents <- TIO.readFile "test_part2.txt"
    print $ sum $ map lastFirstDigitAsNumber (T.lines contents)
