module Main where

import Data.Char
import Data.List
import Data.Maybe


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

numberContainingOrZero :: [NumberPos] -> Int -> Int -> Int
numberContainingOrZero numberPosisitions x y = maybe 0 getNum maybeNumberPos
                                               where maybeNumberPos = find (\numberPos -> (getY numberPos == y) && (getX numberPos <= x) && (x < getX numberPos + getLen numberPos)) numberPosisitions

sumAdjacent :: [NumberPos] -> Int -> Int -> Int
sumAdjacent numberPosisitions x y = numberContainingOrZero numberPosisitions (x - 1) (y + 1)
                                    + numberContainingOrZero numberPosisitions x (y + 1)
                                    + numberContainingOrZero numberPosisitions (x + 1) (y + 1)
                                    + numberContainingOrZero numberPosisitions (x - 1) y 
                                    + numberContainingOrZero numberPosisitions (x + 1) y 
                                    + numberContainingOrZero numberPosisitions (x - 1) (y - 1) 
                                    + numberContainingOrZero numberPosisitions x (y - 1) 
                                    + numberContainingOrZero numberPosisitions (x + 1) (y - 1)

filterNumberPoses :: [NumberPos] -> Int -> Int -> [NumberPos]
filterNumberPoses numberPosisitions x y = numberPosisitions

solveHelper :: String -> [NumberPos] -> Int -> Int -> Int -> Int
solveHelper (char:chars) numberPosisitions x y accumulator
        | null chars = accumulator
        | isDigit char || char == '.' = solveHelper chars numberPosisitions (x + 1) y accumulator
        | char == '\n' = solveHelper chars numberPosisitions 0 (y + 1) accumulator
        | otherwise = solveHelper chars (filterNumberPoses numberPosisitions x y) (x + 1) y (accumulator + sumAdjacent numberPosisitions x y)

solve :: String -> [NumberPos] -> Int
solve string numberPosisitions = solveHelper string numberPosisitions 0 0 0

main :: IO ()
main = do
    contents <- readFile "test_part1.txt"
    print $  solve contents (parseNumbers contents [] 0 0)

