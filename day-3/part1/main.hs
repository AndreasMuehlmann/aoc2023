module Main where

import Data.Char ( digitToInt, isDigit )
import Text.Read.Lex (Number)


data NumberPos = NumberPosCons {getNum :: Int, getX :: Int, getY :: Int, getLen :: Int} deriving Show


getNumberHelper :: String -> Int -> Int -> (Int, Int)
getNumberHelper (char:chars) number len
                    | isDigit char = getNumberHelper chars (number * 10 + digitToInt char) (len + 1)
                    | otherwise = (number, len)

getNumber :: String -> (Int, Int)
getNumber string = getNumberHelper string 0 0

parseNumbers :: String -> [NumberPos] -> Int -> Int-> [NumberPos]
parseNumbers (char:chars) accumulator x y
        | null chars = if isDigit char then NumberPosCons number x y len : accumulator else accumulator
        | isDigit char = parseNumbers (drop (len - 1) chars) digitCell (x + len) y
        | char == '\n' = parseNumbers chars accumulator 0 (y + 1)
        | otherwise = parseNumbers chars accumulator (x + 1) y
        where (number, len) = getNumber (char:chars)
              digitCell = NumberPosCons number x y len : accumulator

isPositionInNumberPos :: NumberPos -> Int -> Int -> Bool
isPositionInNumberPos numberPos x y = (getY numberPos == y) && (getX numberPos <= x) && (x < getX numberPos + getLen numberPos)

relPosPermutations :: [(Int, Int)]
relPosPermutations = [(relX, relY) | relX <- [-1..1], relY <- [-1..1], relY /= 0 || relX /= 0]

numberPosInPermutations :: NumberPos -> Int -> Int -> Bool
numberPosInPermutations numberPos x y = any (\(relX, relY) -> isPositionInNumberPos numberPos (x + relX) (y + relY)) relPosPermutations

filterNumberPoses :: [NumberPos] -> Int -> Int -> [NumberPos]
filterNumberPoses numberPosisitions x y = filter (\numberPosition -> not (any (\(relX, relY) -> isPositionInNumberPos numberPosition (x + relX) (y + relY)) relPosPermutations)) numberPosisitions

solveHelper :: String -> [NumberPos] -> Int -> Int -> Int -> Int
solveHelper (char:chars) numberPosisitions x y accumulator
        | null chars = accumulator
        | isDigit char || char == '.' = solveHelper chars numberPosisitions (x + 1) y accumulator
        | char == '\n' = solveHelper chars numberPosisitions 0 (y + 1) accumulator
        | otherwise = solveHelper chars (filter (\numberPos -> not $ numberPosInPermutations numberPos x y) numberPosisitions) (x + 1) y
                      (accumulator + sum (map getNum (filter (\numberPos -> numberPosInPermutations numberPos x y) numberPosisitions)))

solve :: String -> [NumberPos] -> Int
solve string numberPosisitions = solveHelper string numberPosisitions 0 0 0

main :: IO ()
main = do
    contents <- readFile "part1.txt"
    print $  solve contents (parseNumbers contents [] 0 0)

