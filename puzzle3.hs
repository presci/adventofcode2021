{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
    import qualified Data.Array as A
    import Text.Printf (printf)
    import qualified Data.Foldable as Foldable
    import qualified Data.List as List


    solve ::[[Char]] -> [(Int, Int)]
    solve [] = []
    solve x = solve' $ List.transpose x

    solve' :: [[Char]] -> [(Int, Int)]
    solve' [] = []
    solve' (x:xs) = (find (0, 0) x :  solve' xs)

    find:: (Int, Int) -> [Char] -> (Int, Int)
    find x [] = x
    find (a, b) ('1':xs) = find (a + 1, b) xs
    find (a, b) (_: xs) = find (a, b+1) xs

    datafind :: [(Int, Int)] -> Int
    datafind arr = datafind' [] [] arr
        where
            datafind'::[Int] -> [Int] ->[(Int, Int)] -> Int
            datafind' a b [] = binaryToDecimal2 a * binaryToDecimal2 b
            datafind' a b ((g, e):xss)
                | g >= e = datafind' (a ++ [1]) (b ++ [0]) xss
                | otherwise = datafind' (a ++ [0]) (b ++ [1]) xss

    binaryToDecimal ::[Int] -> Int
    binaryToDecimal [] = 0
    binaryToDecimal x = btd  0 $ reverse x
        where
            btd::Int -> [Int] -> Int
            btd _ [] = 0
            btd j (x:xs) = (x * 2 ^ j)  + btd (j + 1) xs

    binaryToDecimal2 ::[Int] -> Int
    binaryToDecimal2 x = sum $ List.zipWith (\x y -> (2 ^ x) * y ) [0..]  $ reverse x
------- Solve 2

    solve2 ::[[Int]] -> Int
    solve2 arg0 = (binaryToDecimal2 $ oxygen [] arg0) * (binaryToDecimal2 $ co2 [] arg0)

    co2 ::[Int] -> [[Int]] -> [Int]
    co2 acc [] = acc
    co2 acc [x] = acc ++ x
    co2 acc arg0 =solveco2 acc arg0 (findpopular unpopular arg0)
    testco2::[[Int]] -> Bool 
    testco2 [x] = True 
    testco2 _ = False

    oxygen :: [Int] -> [[Int]] -> [Int]
    oxygen acc [] = acc
    oxygen acc [x] = acc ++ x 
    oxygen acc arg0 = solveoxy acc arg0 (findpopular popular arg0)

    solveoxy ::[Int] -> [[Int]] -> Int -> [Int]
    solveoxy acc [] _ = acc
    solveoxy acc arg0 arg1 = oxygen (acc ++ [arg1]) (solve220 arg1 arg0 ) 

    solveco2::[Int] -> [[Int]] -> Int -> [Int]
    solveco2 acc [] _ = acc
    solveco2 acc arg0 arg1 = co2 (acc ++ [arg1]) (solve220 arg1 arg0 ) 

    solve220 :: Int -> [[Int]] -> [[Int]]
    solve220 arg00 arg01=  filter (not.null) . map solve221 $ filter (\x -> List.isPrefixOf [arg00] x) arg01
    solve221 ::[Int] -> [Int]
    solve221 [] = []
    solve221 (_:xs) = xs 

    findpopular :: ((Int, Int) -> [Int] -> Int) -> [[Int]]-> Int
    findpopular f arg0 = f (0, 0) (mytranspose  arg0)
        where
        mytranspose::[[Int]] -> [Int]
        mytranspose arg00 = mytranspose' $ List.transpose arg00
        mytranspose'::[[Int]] -> [Int]
        mytranspose' [] = []
        mytranspose' (x:xs) = x

    popular ::(Int, Int) -> [Int] -> Int
    popular (a, b) []
        | a >= b = 1
        | otherwise = 0
    popular (a, b) (1: xs) = popular (a + 1, b) xs
    popular (a, b) (_: xs) = popular (a, b+ 1) xs

    unpopular ::(Int, Int) -> [Int] -> Int
    unpopular (a, b) []
        | b > a = 1
        | otherwise = 0
    unpopular (a, b) (1: xs) = unpopular (a + 1, b) xs
    unpopular (a, b) (_: xs) = unpopular (a, b+ 1) xs



    readInput :: FilePath -> IO[[Int]]
    readInput = fmap (map readNum .lines ) .readFile

    readNum ::[Char] -> [Int]
    readNum = map (\x -> if x == '0' then 0 else 1 ) 
    main::IO()
    main = do
        arr <-  lines <$> readFile "input01.txt"
        printf "hello world"
