module Main where
    import Text.Printf (printf)

    solve ::[Int] -> Int 
    solve [] = 0
    solve [x] = 0
    solve (a:xs) 
        | a < head xs = 1 + solve xs
        | otherwise = solve xs

    solve2::[Int] -> Int
    solve2 [] = 0
    solve2 [x] = 0
    solve2 (a:b:xs) = solve2' (a + b + head xs) (b:xs)

    solve2' ::Int -> [Int] -> Int 
    solve2' _ [] = 0
    solve2' _ [x] = 0
    solve2' _ [_,_] =0
    solve2' pre (a:b:xs) 
        | pre < k = 1 + solve2' k (b:xs)
        | otherwise = solve2' k  (b:xs)
        where k = a + b + head xs 

    main::IO()
    main = do
        k <- map ( read::String -> Int) . lines  <$> readFile "01.txt"
        putStrLn . printf "%d" $ solve k
        putStrLn . printf "%d" $ solve2 k

