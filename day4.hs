module Main where
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Set as Set
-- | Tuple of cell. Contains the 
-- Parameter 1 is the number on the bingoboard
-- Parameter 2 is the bool value if the number is drawn
-- Parameter 3 is the Maybe Int of drawn number
type Cell = (Int, Bool, Maybe Int)
type BingoBoard = [[Cell]]

newline = T.pack ("\n")

-- | Parses the first line of the file into list of int
parse::T.Text -> [Int]
parse arg0 = map (read::String-> Int) $ map (T.unpack) $ T.splitOn (T.pack ",") arg0

-- | Parses the bingo board. This takes list of lines
-- and parses them into bingo board
-- 22 13 17 11  0
--  8  2 23  4 24
-- 21  9 14 16  7
--  6 10  3 18  5
--  1 12 20 15 19

--  3 15  0  2 22
--  9 18 13 17  5
-- 19  8  7 25 23
-- 20 11 10 24  4
-- 14 21 16 12  6
parsebingos ::[T.Text] -> [BingoBoard]
parsebingos value = map parsebingo value
    where 
        parsebingo::T.Text -> BingoBoard
        parsebingo arg0 = parseStringToInt (map words (map (T.unpack) (T.splitOn newline arg0)))
        parseStringToInt::[[String]] -> BingoBoard
        parseStringToInt = fmap (map cellParse)
        cellParse :: String -> Cell
        cellParse value = ((read::String -> Int) value, False, Nothing)


-- | findWinner takes the list of List of Bingo boards. And tries to find the winner.
-- if any  row or column's number matches the numbers that are drawn would be the winner.
findWinner::[[BingoBoard]] -> BingoBoard
findWinner [] = []
findWinner (x:xs) = 
    case findWinnerHelper x of
        Just v -> v
        _ -> findWinner xs
-- | Helper function to go through each bingo board to find the winner
findWinnerHelper :: [BingoBoard] -> Maybe BingoBoard
findWinnerHelper [] = Nothing
findWinnerHelper (g:gs) =
    case isWinner g of
        True -> Just g
        _ -> findWinnerHelper gs

-- | Solution 2 : To find the last board that wins the bingo. Just line findWinner it takes list of Bingo Boards 
solve2 :: [[BingoBoard]] -> BingoBoard  
solve2 arg0 = findWinner2 ((Set.empty), []) arg0

-- | findWinner2 takes a tuple of Set and BingoBoard accumulator
-- If the BingoBoard is not in the Set it tries to find the winner
-- if the BingoBoard is a winner it adds to accumulator and the set
-- first board in the accumulator is the Last winning
-- The idea here is to discard all the BingoBoard which already have won
findWinner2::((Set.Set String), [BingoBoard]) -> [[BingoBoard]] -> BingoBoard -- parameter 1 is accumulator for bingoboard String and bingoBoard
findWinner2 (_, (x:xs)) [] = x -- Answer is the first element of list Set is ignored
findWinner2 arg0 (x:xss) = findWinner2 (findwinner arg0 x) xss
    where 
        findwinner::(Set.Set String, [BingoBoard]) -> [BingoBoard] -> (Set.Set String, [BingoBoard])
        findwinner acc [] = acc
        findwinner (set, acc) (g:gs) = 
            case (Set.notMember f set) && (isWinner g) of
                True -> findwinner (Set.insert (makereadableBingoBoard "" g) set, (g : acc)) gs
                False -> findwinner (set, acc) gs
            where 
                f :: String
                f = makereadableBingoBoard "" g


-- | makes the string out of the bingo board number to put in the set
makereadableBingoBoard::String -> BingoBoard -> String
makereadableBingoBoard boardStr [] = boardStr
makereadableBingoBoard boardStr (x:xs) = makereadableBingoBoard (boardStr ++ makereadableCells "" x) xs
    where
        makereadableCells::String -> [Cell] -> String
        makereadableCells acc [] = acc
        makereadableCells acc ((val, _, _):gs) = makereadableCells (acc ++  show val) gs

-- | isWinner checks to see if the BingoBoard is the winner
-- It does it by checking all the rows. 
-- Then transposing the BingoBoard and checking all the rows
isWinner::BingoBoard -> Bool
isWinner arg0 = (List.any (== True) $ map isRowWinner arg0) || (List.any (== True) $ map isRowWinner $ List.transpose arg0)

-- | isRowWinner checks if all the Cells in the row are winner
isRowWinner::[Cell] -> Bool
isRowWinner arg0 = List.all (== True)  (map cell arg0)
    where 
        cell::Cell -> Bool
        cell (_, True, _) = True
        cell _ = False

-- | findAnswer Sums all the number that are marked False & multiplies it by the last drawn number
findAnswer ::BingoBoard -> Int
findAnswer value@(x:xs) = (*)  (findbaseValue x) (findFalseCell 0 value)
    where 
        findFalseCell::Int -> BingoBoard -> Int
        findFalseCell acc [] = acc
        findFalseCell acc (x:xs) =  findFalseCell (sum' acc x) xs
        sum'::Int -> [Cell] -> Int
        sum' acc [] = acc
        sum' acc ((a, False, _): gs) = sum' (acc + a) gs
        sum' acc ((_, True, _): gs) = sum' acc gs        
        findbaseValue ::[Cell] -> Int
        findbaseValue [] = 0
        findbaseValue [(_,_, Just v)] = v
        findbaseValue ((_,_,Nothing): xs) = 0
        findbaseValue ((_,_, Just v): xs) = v

-- | This creates the infinite list of Bingo boards for all the numbers that are drawn
drawBingo ::[Int] -> [BingoBoard] -> [[BingoBoard]]
drawBingo [] _ = []
drawBingo (x:xs) bingoBoards = k: drawBingo xs k
    where 
        k = updateBingoBoards x bingoBoards

-- | Creates Bingoboards for each number drawn
updateBingoBoards ::Int -> [BingoBoard] -> [BingoBoard]
updateBingoBoards draw boards = map (\x -> updateBingoBoard draw x) boards

-- | Matches each cell of the bingo board if it is found makes the Cell value true
-- and all the cell with number drawn
updateBingoBoard :: Int -> BingoBoard -> BingoBoard
updateBingoBoard arg0 arg1 = fmap (map flipcell) arg1
    where
        flipcell ::Cell -> Cell
        flipcell value@(a, b, _)  
            | a == arg0 && b == False = (a, True, Just arg0)
            | otherwise = (a, b, Just arg0)


main::IO()
main = do
    content <- readFile "input.txt"
    let input =  T.splitOn (T.pack "\n\n") $ T.pack content
    let draws = parse (head input)  
    let bingoboards = parsebingos (tail input)
    let game = drawBingo draws bingoboards
    let updated = findAnswer $ findWinner $ game
    print updated
    let winner2 =  findAnswer $ solve2  game
    print winner2
