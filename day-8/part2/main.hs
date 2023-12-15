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

doInstruction :: Bool -> [Node] -> Node -> Node
doInstruction goLeft nodes node
        | goLeft = nodes!!head (edges node)
        | otherwise = nodes!!last (edges node)

getStepsWithSameInstruction :: Int -> Int -> [Int]
getStepsWithSameInstruction steps instructionLength = [steps - instructionLength, steps - 2*instructionLength..0]

getLoop :: [String] -> Int -> Maybe (Int, Int)
getLoop visitedNodes lengthInstructions
        | isJust maybeIndexLoopStart = Just (stepsSameInstruction!!fromJust maybeIndexLoopStart, length visitedNodes - 1)
        | otherwise = Nothing
        where stepsSameInstruction = getStepsWithSameInstruction (length visitedNodes) lengthInstructions
              nodesSameInstruction = map (visitedNodes!!) stepsSameInstruction
              maybeIndexLoopStart = elemIndex (last visitedNodes) nodesSameInstruction


findLoop :: String -> [Node] -> Int -> [String]-> Node  -> ([String], (Int, Int))
findLoop instructions nodes  steps previousSteps node
        | isJust loop = (drop (fst (fromJust loop) - 5) visitedNodes, fromJust loop)
        | otherwise = findLoop instructions nodes (steps + 1) visitedNodes nextNode
        where currentInstruction = instructions!!(steps `mod` length instructions)
              nextNode = doInstruction (currentInstruction == 'L') nodes node
              visitedNodes = previousSteps ++ [name node]
              loop = getLoop visitedNodes (length instructions)

solveHelper :: String -> [Node] -> [Node] -> Int -> Int
solveHelper instructions nodes currentNodes steps
        | all (\node -> last (name node) == 'Z') currentNodes = steps
        | otherwise = solveHelper instructions nodes (map (doInstruction (currentInstruction == 'L') nodes) currentNodes) (steps + 1)
        where currentInstruction = instructions!!(steps `mod` length instructions)

solve :: (String, [Node]) -> ([String], (Int, Int))
solve (instructions, nodes) = findLoop instructions nodes 0 [] $ (filter (\node -> last (name node) == 'A') nodes)!!1

main :: IO ()
main = do
    contents <- TIO.readFile "part2.txt"
    print $ solve $ parseMap contents

-- loop length probably wrong by 2
-- no ..Z in loop