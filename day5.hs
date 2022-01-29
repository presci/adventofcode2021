module Main where
import qualified Data.Map as M
import qualified Data.List as L
import qualified Text.ParserCombinators.ReadP as R
import Control.Applicative
import Data.Char as C
import Control.Monad

type Point = (Int, Int)
-- | helper function 
add :: Int -> Int
add = (+1)
sub :: Int -> Int
sub arg0 = arg0 -1

-- | parses the Digit
parseDigits :: R.ReadP String
parseDigits = R.many1 (R.satisfy C.isDigit )

-- | reads the Int
parseInt :: R.ReadP Int
parseInt = read <$> parseDigits

-- | parses int with spaces
parseIntSpaces :: R.ReadP Int
parseIntSpaces = R.skipSpaces *> parseInt <* R.skipSpaces

-- | parses the coordinates 
-- Coordinates are string 
-- 226,467 -> 226,937
-- 587,330 -> 587,51
-- 487,797 -> 924,797
-- 216,11 -> 216,875
-- 316,263 -> 301,263
parseCoord :: R.ReadP (Int, Int, Int, Int)
parseCoord = do
  x1 <- parseIntSpaces 
  y1 <- R.char ',' >> parseIntSpaces
  x2 <- R.string "->" >> parseIntSpaces
  y2 <- R.char ',' >> parseIntSpaces <* R.eof
  return (x1, y1, x2, y2)

-- | this is the first problem and 2 problem
problem01 :: [String] -> Int
problem01  arg0 = length $ solve01 M.empty arg0


-- | Converts the map to list with all the points which have value
-- greater than 2
solve01::(M.Map Point Int ) ->  [String] -> [(Point, Int)]
solve01 acc [] = M.toList $ M.filter ( > 1)  acc
solve01 mp (x:xs) = solve01 (addtomap mp  ( R.readP_to_S parseCoord x)) xs

-- | creates the Map with all the points returns the map
addtomap :: (M.Map Point Int) -> [((Int, Int, Int, Int), String)] -> M.Map Point Int
addtomap arg0 (((x1,y1,x2,y2), _):_) 
  | x1 == x2 && y1 >= y2 = updatemap arg0 [(i, j) | i <- [x1], j <- [y2..y1]]
  | x1 == x2 && y1 < y2 = updatemap arg0 [(i, j) | i <- [x1], j <- [y1..y2]]
  | x1 >= x2 && y1 == y2 = updatemap arg0 [(i, j) | i <- [x2..x1], j <- [y1]]
  | x1 < x2 && y1 == y2 = updatemap arg0 [(i, j) | i <- [x1..x2], j <- [y1]]
  | otherwise = updatemap arg0 $ generateCoord (x1, y1, x2, y2) xf yf  -- Removing this would be answer for 1st problem
    where 
      xf = if x1 < x2 then add else sub
      yf = if y1 < y2 then add else sub

-- | generateCoord This function is used for creating diagonal lines
-- takes a coordiate and 2 functions add or subtract and generates the points
generateCoord :: (Int, Int, Int, Int) -> (Int -> Int) -> (Int -> Int)-> [Point]
generateCoord (a, b, c, d)  xf yf 
  | a == c = [(a,b)]
  | otherwise = (a, b): generateCoord (xf a, yf b , c , d) xf yf

-- | Update the map if the point is found +1 to it or 1
updatemap ::(M.Map Point Int) -> [(Int, Int)] -> M.Map Point Int
updatemap map [] = map
updatemap map (x:xs) = updatemap (update' map x) xs
  where
    update' :: M.Map Point Int -> (Int, Int) -> M.Map Point Int
    update' m key = case M.lookup key m of
      Just value -> M.insert key (value + 1) m
      _ -> M.insert key 1 m

main::IO()
main = do
  g <- lines <$> readFile "input.txt"
  putStrLn $  show $ problem01 g
  
