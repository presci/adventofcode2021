module Main where
    import Text.Printf (printf)

    parse:: String -> (String, Int)
    parse a = g $ words a
        where 
            g::[String] -> (String, Int)
            g [] = ("", 0)
            g [a, b]= (a, read b ::Int)
            g (a:b:_)= (a, read b::Int)

    solve ::(Int, Int) -> [(String, Int)] -> Int
    solve (a, b) [] = a * b
    solve (h, d) (x:xs) = 
        case x of {
            ("forward", val) -> solve (h + val, d) xs;
            ("up", val) -> solve (h, d - val) xs;
            ("down", val) -> solve(h, d + val) xs;
            (_, _) -> solve (h, d) xs
        }

    solve2::(Int, Int, Int) -> [(String, Int)] -> Int 
    solve2 (h, d, _) [] = h * d 
    solve2 (h, d, a) (x:xs) = 
        case x of {
            ("forward", val) -> solve2 (h + val, d + (a * val), a) xs;
            ("up", val) -> solve2 (h, d, a - val) xs;
            ("down", val) -> solve2 (h, d, a + val) xs;
            (_, _) -> solve2 (h, d, a) xs
        }

    main::IO()
    main = do
        question <- map parse . lines <$> readFile "2021.02.txt"
        printf "%d\n" $ solve (0, 0) question
        printf "%d\n" $ solve2 (0, 0, 0) question
