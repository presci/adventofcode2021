module Main where
import qualified Data.Vector as V
import qualified Data.List as L

solve01 :: [String] -> Int
solve01 = foldr (\x y -> isValid x + y) 0 

strings = ["[({(<(())[]>[[{[]{<()<>>",
           "[(()[<>])]({[<{<<[]>>(",
           "{([(<{}[<>[]}>{[]{[(<()>",
           "(((({<>}<{<{<>}{[]{[]{}",
           "[[<[([]))<([[{}[[()]]]",
           "[{[{({}]{}}([{[{{{}}([]",
           "{<[[]]>}<{[{[{[]{()[[[]",
           "[<(<(<(<{}))><([]([]()",
           "<{([([[(<>()){}]>(<<{{",
           "<{([{{}}[<[[[<>{}]]]>[]]"
          ]

isValid::String ->  Int
isValid = isValid' []
  where
    isValid' :: String -> String -> Int
    isValid' _ [] = 0
    isValid' ('(':acc) (')':rem) = isValid' acc rem
    isValid' ('[':acc) (']':rem) = isValid' acc rem
    isValid' ('{':acc) ('}':rem) = isValid' acc rem
    isValid' ('<': acc) ('>': rem) = isValid' acc rem
    isValid' acc (x:rem)
      | x == ')' = 3
      | x == ']' = 57
      | x == '}' = 1197
      | x == '>' = 25137
      | otherwise = isValid' (x:acc) rem



solve02 :: [String] ->  Int
solve02 = getMiddle . V.fromList . filter (/= 0) . L.sort  . map isValid2
  where
    getMiddle :: V.Vector Int -> Int
    getMiddle arg = arg V.! (V.length arg `div` 2)

      
isValid2::String -> Int
isValid2 = calculate . convert . isValid' []
  where
    isValid' :: String -> String -> String
    isValid' acc [] = acc
    isValid' ('(':acc) (')':rem) = isValid' acc rem
    isValid' ('[':acc) (']':rem) = isValid' acc rem
    isValid' ('{':acc) ('}':rem) = isValid' acc rem
    isValid' ('<': acc) ('>': rem) = isValid' acc rem
    isValid' acc (x:rem)
      | x == '(' || x == '[' || x == '{' || x == '<' = isValid' (x:acc) rem
      | otherwise = ""
    calculate :: [Int] -> Int
    calculate = foldl (\x y -> ( x * 5) + y) 0 

convert :: String -> [Int]
convert = map (\x -> case x of
                  '(' -> 1
                  '[' -> 2
                  '{' -> 3
                  '<' -> 4
                  _ -> 0) 

main::IO()
main = do
  arr <- lines <$> readFile "input.txt"
  print $ solve01 arr
  print $ solve02 arr
