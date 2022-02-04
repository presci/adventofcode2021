module Main where
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L

                                                                                                                                                                                                                                                             
getData::String -> Int                                                                                                                                                                                                                                       
getData arg0 = sum $ map (\x -> if T.length x == 2 || T.length x == 4 || T.length x == 3 || T.length x == 7 then 1 else 0)  $ splitBySpace $ getLast $ splitDataByPipe                                                                                       
  where                                                                                                                                                                                                                                                      
    splitDataByPipe :: [T.Text]                                                                                                                                                                                                                              
    splitDataByPipe = T.splitOn (T.pack "|") (T.pack arg0)                                                                                                                                                                                                   
    getLast ::[T.Text] -> T.Text                                                                                                                                                                                                                             
    getLast [x] = x                                                                                                                                                                                                                                          
    getLast (x:xs) = getLast xs                                                                                                                                                                                                                              
    splitBySpace :: T.Text -> [T.Text]                                                                                                                                                                                                                       
    splitBySpace = T.splitOn (T.pack " ")                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                             
solve01::[String] -> Int                                                                                                                                                                                                                                     
solve01 [] = 0                                                                                                                                                                                                                                               
solve01 (x:xs) = getData x + solve01 xs                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                             
{--                                                                                                                                                                                                                                                          
0: 6  6 - 2 = 4                                                                                                                                                                                                                                              
1: 2*
3: 5 if 5 letter contains letter from letter 1 
4: 4*
5: if it doesn't contain letters from 1 it is 5
6: 6  6 - 2= 5
7: 3*
8: 7* 
9: 6 combine 3 & 4 and remove all from 6 letter if you get empty list it is 9



  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg


cf -> 1
acf -> 7 
bdcf -> 4
adgcf, adgec, adgbf -> [adg] -> 3,2,5
abcdfg, abdefg,abcefg -> 96

--}


getn 1 arg0 = L.sort . head $ filter(\x -> length x == 2) arg0
getn 4 arg0 = L.sort . head $ filter(\x -> length x == 4) arg0
getn 7 arg0 = L.sort . head   $ filter (\x -> length x == 3) arg0
getn 8 arg0 = L.sort. head $ filter (\x -> length x == 7) arg0
getn 3 arg0  = get3' $ get5Count arg0
  where
    get3' :: [String] -> String
    get3' [x] = L.sort x
    get3' (x:xs) = case common [x, one] == one of
      True -> L.sort x
      _ -> get3' xs
    one = getn 1 arg0
getn 2 arg0 = get2' $ get5Count arg0
  where
    get2' ::[String] -> String
    get2' [x] = L.sort x
    get2' (x:xs) = case length (common [x, four]) == 2 of
      True -> L.sort x
      _ -> get2' xs
    four = getn 4 arg0
getn 5 arg0 = get5'  [getn 2 arg0, getn 3 arg0] (get5Count arg0)
  where
    get5'::[String] -> [String] -> String
    get5' _ [x] = L.sort x
    get5' st (x:xs) = case x `notElem` st of
      True -> x
      _ ->  get5' st xs
getn 9 arg0 = get9' $ get6Count arg0
  where
    get9'::[String] -> String
    get9' [x] = x
    get9' (x:xs) = case length ( common [x, four]) == 4 of
      True -> L.sort x
      _ -> get9' xs
    four = getn 4 arg0
getn 0 arg0 = get0' $ filter (\x -> x /= (getn 9 arg0)) ( get6Count arg0)
  where
    get0'::[String] -> String
    get0' [x] = x
    get0' (x:xs) = case length ( common [x, one]) == 2 of
      True -> L.sort x
      _ -> get0' xs
    one = getn 1 arg0
getn 6 arg0 = get6' $ get6Count arg0
  where
    get6'::[String] -> String
    get6' [x] = x
    get6' (x:xs) = case length ( common [x, one]) == 1 of
      True -> L.sort x
      _ -> get6' xs
    one = getn 1 arg0
   
get5Count ::[String] -> [String]
get5Count arg0 = map L.sort $ filter (\x -> length x == 5) arg0
get6Count ::[String] -> [String]
get6Count arg0 = map L.sort $ filter (\x -> length x == 6) arg0

common [x] = x
common (x:xs) = filter( `elem` x) $ common xs


getMappedValues ::[String] -> [(String, Int)]
getMappedValues arg0 = map (\x -> (getn x arg0, x) ) [0,1,2,3,4,5,6,7,8,9]


getValues::String-> String -> Int
getValues arg0 arg1 = foldl (\acc (_, d) -> (acc * 10) + d) 0 $ findValues (getMappedValues (words arg0)) (words arg1)
  where
    findValues ::[(String, Int)] -> [String] -> [(String, Int)]
    findValues _ [] = []
    findValues arg0 (x:xs) =  head (filter (\(c, d) -> if c == (L.sort x) then True else False) arg0) : findValues arg0 xs 


solve02::[String] -> Int
solve02 arg0 = sum $ map crab arg0
  where
    crab::String -> Int
    crab arg0 = light01' $ map (T.unpack) (T.splitOn (T.pack "|") (T.pack arg0))
    light01' ::[String] -> Int
    light01' [] = 0
    light01' (x:b:_)= getValues x b



main:: IO()
main = do
  k <- lines <$> readFile "input.txt"
  let f = solve01 k
  print f
  let g = solve02 k
  print g
