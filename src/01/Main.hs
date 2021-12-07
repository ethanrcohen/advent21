module Main where

part1 :: [Int] -> Int
part1 [] = 0
part1 [x] = 0
part1 (x:xs) 
  | x < head xs = 1 + part1 xs
  | otherwise = part1 xs

part2 :: [Int] -> Int
part2 [] = 0
part2 [x, y] = 0
part2 [x, y, z] = 0
part2 (x:y:z:xs) 
  | x < head xs = 1 + part2 (y:z:xs)
  | otherwise = part2 (y:z:xs)

readInt :: String -> Int
readInt = read

main = do 
        -- figure out how to specify path better
        input <- readFile "/Users/ecohen/Developer/advent21/src/01/input.txt"
        let nums = map readInt . words $ input
        let part1Ans = part1 nums
        print("part1: " ++ show part1Ans)
        let part2Ans = part2 nums
        print("part2: " ++ show part2Ans)
