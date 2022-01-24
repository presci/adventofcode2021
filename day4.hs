module Main where
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Set as Set

type Cell = (Int, Bool, Maybe Int)

type BingoBoard = [[Cell]]

newline = T.pack ("\n")
{-- File Parsing --}
parse::T.Text -> [Int]
parse arg0 = map (read::String-> Int) $ map (T.unpack) $ T.splitOn (T.pack ",") arg0

parsebingos ::[T.Text] -> [BingoBoard]
parsebingos value = map parsebingo value

parsebingo::T.Text -> BingoBoard
parsebingo arg0 = parseStringToInt (map words (map (T.unpack) (T.splitOn newline arg0)))

parseStringToInt::[[String]] -> BingoBoard
parseStringToInt = fmap (map cellParse)

cellParse :: String -> Cell
cellParse value = ((read::String -> Int) value, False, Nothing)


{-- find winner --}
findWinner::[[BingoBoard]] -> BingoBoard
findWinner [] = []
findWinner (x:xs) = 
    case findWinnerHelper x of
        Just v -> v
        _ -> findWinner xs

findWinnerHelper :: [BingoBoard] -> Maybe BingoBoard
findWinnerHelper [] = Nothing
findWinnerHelper (g:gs) =
    case isWinner g of
        True -> Just g
        _ -> findWinnerHelper gs


solve2 :: [[BingoBoard]] -> BingoBoard  -- Starts to solve2 problem. Takes infinite list of BingoBoard &
solve2 arg0 = findWinner2 ((Set.empty), []) arg0

-- findWinner2 takes a tuple of Set and BingoBoard accumulator
-- If the BingoBoard is not in the Set it tries to find the winner
-- if the BingoBoard is a winner it adds to accumulator and the set
-- first board in the accumulator is the Last winning
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


-- makes the string out of the bingo board number to put in the set
makereadableBingoBoard::String -> BingoBoard -> String
makereadableBingoBoard boardStr [] = boardStr
makereadableBingoBoard boardStr (x:xs) = makereadableBingoBoard (boardStr ++ makereadableCells "" x) xs
    where
        makereadableCells::String -> [Cell] -> String
        makereadableCells acc [] = acc
        makereadableCells acc ((val, _, _):gs) = makereadableCells (acc ++  show val) gs


isWinner::BingoBoard -> Bool
isWinner arg0 = (List.any (== True) $ map isRowWinner arg0) || (List.any (== True) $ map isRowWinner $ List.transpose arg0)

isRowWinner::[Cell] -> Bool
isRowWinner arg0 = List.all (== True)  (map cell arg0)
    where 
        cell::Cell -> Bool
        cell (_, True, _) = True
        cell _ = False

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


{-- draw bingo --}
drawBingo ::[Int] -> [BingoBoard] -> [[BingoBoard]]
drawBingo [] _ = []
drawBingo (x:xs) bingoBoards = k: drawBingo xs k
    where 
        k = updateBingoBoards x bingoBoards
     

updateBingoBoards ::Int -> [BingoBoard] -> [BingoBoard]
updateBingoBoards draw boards = map (\x -> updateBingoBoard draw x) boards

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
