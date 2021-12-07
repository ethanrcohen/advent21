module Main where

import Data.List

part1 :: [String] -> Int
part1 s = (\(x,y) -> x*y) . foldl (\(x1, y1) (x2,y2) -> (x1+x2, y1+y2)) (0,0) $  map part1Step s 

part1Step :: String -> (Int, Int)
part1Step line 
  | dir == "forward" = (x, 0)
  | dir == "down" = (0, x)
  | dir == "up" = (0, -1 * x)
  | otherwise = (0,0)
  where [dir, numStr]  = words line
        x = readInt numStr

part2 :: [String] -> Int 
part2 s = (\(x, y, z) -> x*y) $ foldl part2Step (0, 0, 0) s

part2Step :: ( (Int, Int, Int) -> String -> (Int, Int, Int) )
part2Step (x0, y0, aim0) line 
       | dir == "forward" = (x0 + magnitude, y0 + aim0 * magnitude, aim0)
       | dir == "down" = (x0, y0, aim0 + magnitude)
       | dir == "up" = (x0, y0, aim0 - magnitude)
       where [dir, numStr] = words line
             magnitude = readInt numStr


readInt :: String -> Int
readInt = read

main = do
        input <- readFile "/Users/ecohen/Developer/advent21/src/02/input.txt"
        let part1Ans = part1 $ lines input
        print ("part1: " ++ show part1Ans)
        let part2Ans = part2 $ lines input
        print ("part2: " ++ show part2Ans)
