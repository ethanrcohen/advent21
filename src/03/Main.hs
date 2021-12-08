module Main where

import Data.Char

zeroes :: [Int]
zeroes = 0 : zeroes

digits :: Int -> [Int]
digits = map digitToInt . show

addDigit :: Int -> Int -> Int 
addDigit acc digit = 10*acc + digit


part1 :: [[Char]] -> Int
part1 xs = let counts = foldl part1Fold (repeat 0) xs
                len = length xs 
                gamma = binaryToDecimal . foldl addDigit 0 $ map (\x -> if (fromIntegral x) > (fromIntegral len / 2) then 1 else 0) counts
                epsilon = binaryToDecimal . foldl addDigit 0 $ map (\x -> if (fromIntegral x) > (fromIntegral len / 2) then 0 else 1) counts
           in epsilon * gamma

part1Fold :: [Int] -> [Char] -> [Int]
part1Fold xs ys = zipWith (\x y -> x + digitToInt y) xs ys 

readInt :: String -> Int
readInt = read

binaryToDecimal :: Int -> Int 
binaryToDecimal 0 = 0
binaryToDecimal x = 2 * binaryToDecimal (div x 10) + (mod x 10)



main = do 
        input <- readFile "/Users/ecohen/Developer/advent21/src/03/input.txt"
        let part1Ans = part1 $ lines input
        print ("part1: " ++ show part1Ans)
