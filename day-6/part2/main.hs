module Main where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO


textToInt :: T.Text -> Int
textToInt text = case TR.decimal text of
    Left _err -> -1
    Right (result, _) -> result

parseLineWithIdentifierConcatedInt :: T.Text -> Int
parseLineWithIdentifierConcatedInt = textToInt . T.concat . tail . T.words

parseLongRace :: T.Text -> (Int, Int)
parseLongRace contents = (parseLineWithIdentifierConcatedInt $ head $ T.lines contents, parseLineWithIdentifierConcatedInt $ last $ T.lines contents)

solveRace :: (Int, Int) -> Int
solveRace (time, record) = ceiling ((fromIntegral (-time) - sqrt discriminatory) / (-2) - 1) - floor ((fromIntegral (-time) + sqrt discriminatory) / (-2) + 1) + 1
        where discriminatory = fromIntegral $ time^2 - 4 * record

main :: IO ()
main = do
    contents <- TIO.readFile "part2.txt"
    print $ solveRace $ parseLongRace contents