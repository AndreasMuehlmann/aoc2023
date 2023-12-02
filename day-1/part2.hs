import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char
import System.IO


firstDigit :: T.Text -> Char
firstDigit line = T.head firstNotDigitsDropped
    where firstNotDigitsDropped = T.dropWhile (not . isDigit) line

lastDigit :: T.Text -> Char
lastDigit line = T.last lastNotDigitsDropped
    where lastNotDigitsDropped = T.dropWhileEnd (not . isDigit) line

textDigitsToDigits :: T.Text -> T.Text
textDigitsToDigits = T.replace (T.pack "one") (T.pack "one1one")
                     . T.replace (T.pack "two") (T.pack "two2two")
                     . T.replace (T.pack "three") (T.pack "three3three")
                     . T.replace (T.pack "four") (T.pack "four4four")
                     . T.replace (T.pack "five") (T.pack "five5five")
                     . T.replace (T.pack "six") (T.pack "six6six")
                     . T.replace (T.pack "seven") (T.pack "seven7seven")
                     . T.replace (T.pack "eight") (T.pack "eight8eight")
                     . T.replace (T.pack "nine") (T.pack "nine9nine")

lastFirstDigitAsNumber :: T.Text -> Integer
lastFirstDigitAsNumber line = read concatedDigits :: Integer
    where lineWithTextDigitsAsDigits = textDigitsToDigits line
          concatedDigits = [firstDigit lineWithTextDigitsAsDigits, lastDigit lineWithTextDigitsAsDigits]

main :: IO ()
main = do
    contents <- TIO.readFile "test_part2.txt"
    print $ sum $ map lastFirstDigitAsNumber (T.lines contents)
