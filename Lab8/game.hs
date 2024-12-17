import Control.DeepSeq (NFData)
import Data.IntMap (insert)
import Language.Haskell.TH (letE)
import GHC.Generics (U1(U1))
-- Position

type Position = (Int, Int)


-- Direction
data Direction = W | E | S | N
    deriving Show


-- Board

data Board = Board {
     position :: Position,
     direction :: Direction,
     marked :: [Position]
    } deriving Show


inBoard :: Position -> Bool
inBoard (x, y) = x >= 0 && y >= 0

update :: Board -> Char -> Board
--update (Board (x, y) d m) _= Board (x + 1, y) d m 
--robimy dopasowanie wzorca jako haskellowy switch case
update (Board p _ m) 'w' = Board p W m
update (Board p _ m) 'e' = Board p E m
update (Board p _ m) 's' = Board p S m
update (Board p _ m) 'n' = Board p N m
update (Board (x, y) W m) 'f' = Board (x - 1, y) W m
update (Board (x, y) E m) 'f' = Board (x + 1, y) E m
update (Board (x, y) S m) 'f' = Board (x, y + 1) S m
update (Board (x, y) N m) 'f' = Board (x, y - 1) N m
update (Board p d m) 'm' =
    Board p d (if inBoard p then insertUnique p m else m)
update b _ = b

insertUnique :: Eq a => a -> [a] -> [a]
insertUnique x [] = [x]
insertUnique x (h:t) =
    if x == h then h:t
    else h:insertUnique x t


updates :: Board -> [Char] -> Board
--updates = foldl (\b c -> update b c)
updates = foldl update

initBoard :: Board
initBoard = Board (0, 0) E []

showField :: Board -> Position -> Char
showField (Board p d m) fp
  | fp == p = case d of
            N -> '^'
            S -> 'v'
            E -> '>'
            W -> '<'
  | inBoard fp = if elem fp m then '.'
            else '#'
  | otherwise = '~'

showRow :: Board -> Int -> String
showRow b row =
    let
        xp = fst (position b)
    in
    --    map (\x -> showField b (x, row)) [xp - 10 .. xp + 10]
    [showField b (x, row) | x <- [xp - 10 .. xp + 10]] -- list comprehension zamiast map

-- let xp = fst(position b)
-- [showField b(xp - 10, row), .... , showField b (xp + 10, row)] 

rows :: Board -> [String]
rows b =
    let
        yp = snd (position b)
    in
        [showRow b y | y <- [yp - 5 .. yp + 5]]

showBoard :: Board -> String
showBoard = unlines . rows

showBoardState :: Board -> String
showBoardState b@(Board (x, y) _ _) =
    "(" ++ show x ++ ", " ++ show y ++ ")\n" ++ showBoard b



game :: Board -> IO ()
game board = do
    putStr (showBoardState board)
    commandLine <- getLine
    if elem 'q' commandLine then
        return () -- return to opakowywanie w monadę 
    else 
        game $ updates board commandLine

main :: IO ()
main = do
    game initBoard

