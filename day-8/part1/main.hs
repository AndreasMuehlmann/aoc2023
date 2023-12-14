module Main where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO

import Data.Maybe
import Data.List

data Node = Node {name :: String, edges :: [Int]} deriving Show

onlySpaces :: T.Text -> T.Text
onlySpaces = T.replace (T.pack "=") (T.pack " ") . T.replace (T.pack "(") (T.pack " ")
             . T.replace (T.pack ")") (T.pack " ") . T.replace (T.pack ",") (T.pack " ")

parseNodeAsTuple :: T.Text -> (String, [String])
parseNodeAsTuple line = (head nameAndEdges, tail nameAndEdges)
        where nameAndEdges = words $ T.unpack $ onlySpaces line

toIndex :: [String] -> String -> Int
toIndex nodeNames edge = fromJust maybeIndex
        where maybeIndex = elemIndex edge nodeNames

toNodes :: Int -> [Node] -> [(String, [String])]-> [Node]
toNodes index nodes tupleNodes
        | index >= length tupleNodes = nodes
        | otherwise = toNodes (index + 1) (nodes ++ [Node (fst $ tupleNodes!!index) $ map (toIndex nodeNames) (snd $ tupleNodes!!index)]) tupleNodes
        where nodeNames = map fst tupleNodes

parseMap :: T.Text -> (String, [Node])
parseMap text = (T.unpack $ head linesText, toNodes 0 [] $ map parseNodeAsTuple $ tail $ tail linesText)
        where linesText = T.lines text

solveHelper :: String -> [Node] -> Node -> Int -> Int
solveHelper instructions nodes node steps
        | name node == "ZZZ" = steps
        | currentInstruction == 'R' = solveHelper instructions nodes (nodes!!last (edges node)) (steps + 1)
        | otherwise = solveHelper instructions nodes (nodes!!head (edges node)) (steps + 1)
        where currentInstruction = instructions!!(steps `mod` length instructions)

solve :: (String, [Node]) -> Int
solve (instructions, nodes) = solveHelper instructions nodes (head $ filter (\node -> name node == "AAA") nodes) 0
        where namesNodes = map name nodes

main :: IO ()
main = do
    contents <- TIO.readFile "part1.txt"
    print $ solve $ parseMap contents