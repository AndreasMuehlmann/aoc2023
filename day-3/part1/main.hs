module Main where

import Data.Char

getNumberHelper :: [String] -> Int -> Int -> Int -> String
getNumberHelper stringLines x y direction 
                | isDigit char && direction < 0 = getNumberHelper stringLines (x + direction) y direction ++ [char]
                | isDigit char && direction > 0 = char : getNumberHelper stringLines (x + direction) y direction
                | otherwise = []
                where char = (stringLines!!y)!!x 
 

getNumber :: [String] -> Int -> Int -> String
getNumber stringLines x y = getNumberHelper stringLines (x - 1) y (-1) ++ getNumberHelper stringLines x y 1

solve :: String -> Int -> Int -> Int -> Int
solve (char:chars) x y accumulator
                | isDigit char || (char == '.') = solve chars (x + 1) y accumulator
                | char == '\n' = solve chars 0 (y + 1) accumulator
                | null chars = accumulator -- sum . map getNumber . map position + rel_positions
                | otherwise = 0 --solve  chars (x + 1) y (accumulator + (sum . map getNumber . map position + rel_positions))
                where rel_positions = [(relative_x, relative_y) | relative_x <- [-1, 0, 1], relative_y <- [-1, 0, 1], relative_y /= 0 && relative_x /= 0]
main :: IO ()
main = do
    contents <- readFile "test_part1.txt"
    print $ solve contents 0 0 0
