module Main where

import Data.Char

addDigit :: Int -> Int -> Int 
addDigit acc digit = 10*acc + digit

part1 :: [[Char]] -> Int
part1 xs = let counts = foldl part1Fold (repeat 0) xs
               len = length xs 
               gamma = binaryToDecimal . foldl addDigit 0 $ map (\x -> if (fromIntegral x) > (fromIntegral len / 2) then 1 else 0) counts
               epsilon = binaryToDecimal . foldl addDigit 0 $ map (\x -> if (fromIntegral x) > (fromIntegral len / 2) then 0 else 1) counts
           in epsilon * gamma

part1Fold :: [Int] -> [Char] -> [Int]
part1Fold = zipWith (\x y -> x + digitToInt y) 

readInt :: String -> Int
readInt = read

binaryToDecimal :: Int -> Int 
binaryToDecimal 0 = 0
binaryToDecimal x = 2 * binaryToDecimal (div x 10) + (mod x 10)

oxygen :: [[Char]] -> Int -> [Char] 
oxygen [] _ = []
oxygen (x:[]) _  = x
oxygen xs ind = let count = sum $  map (digitToInt . (\x -> x !! ind)) xs
                    len = length xs
                    c = if fromIntegral count >= (fromIntegral len / 2) then '1' else '0'
                    filtered = filter (\x -> x !! ind == c) xs
                  in oxygen filtered (ind + 1)

co2 :: [[Char]] -> Int -> [Char] 
co2 [] _ = []
co2 (x:[]) _  = x
co2 xs ind = let count = sum $  map (digitToInt . (\x -> x !! ind)) xs
                 len = length xs
                 c = if fromIntegral count < (fromIntegral len / 2) then '1' else '0'
                 filtered = filter (\x -> x !! ind == c) xs 
               in co2 filtered (ind + 1)


part2 :: [[Char]] -> Int 
part2 xs = let oxygenVal = binaryToDecimal . readInt $ oxygen xs 0
               co2Val = binaryToDecimal . readInt $ co2 xs 0 
             in oxygenVal * co2Val

main = do 
        input <- readFile "/Users/ecohen/Developer/advent21/src/03/input.txt"
        let part1Ans = part1 $ lines input
        print ("part1: " ++ show part1Ans)
        let part2Ans = part2 $ lines input
        print ("part2: " ++ show part2Ans)
