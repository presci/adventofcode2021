module Main where
import qualified Data.List as L
import Data.Function (on)
import Data.Ord (comparing)
import Control.Arrow


createdata::String -> [(Int, Int)]
createdata = map (head &&& length ) . L.groupBy (==) . L.sort . map (read ::String-> Int) . split ',' 

split::Char -> [Char] -> [[Char]]
split c xs = case break (== c)  xs of
  (value, []) -> [value]
  (value, x:gx) -> value : split c gx

updateEveryDay :: [(Int, Int)] -> [(Int, Int)]
updateEveryDay  =  map (foldr (\(x1, y1) (_, y2) -> (x1, y1+y2)) (0,0)) . L.groupBy ((==) `on` fst) . L.sortBy (comparing fst) . concatMap step

step (0, n) = [(6, n), (8, n)]
step (day, n) = [(day - 1, n)]

countdown ::Int -> [(Int, Int)] -> [(Int, Int)]
countdown 0 t = t
countdown c arg0 = countdown (c-1) $ updateEveryDay arg0

problem1 ::String -> Int
problem1  arg0 =  foldr (\(_, x) acc -> acc + x) 0 $ countdown 256 $ createdata arg0

main ::IO()
main = do 
  putStrLn "hello world"
