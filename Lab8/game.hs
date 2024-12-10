import Control.DeepSeq (NFData)
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


update :: Board -> Char -> Board
update (Board (x, y) d m) _= Board (x + 1, y) d m 

updates :: Board -> [Char] -> Board
--updates = foldl (\b c -> update b c)
updates = foldl update

initBoard :: Board
initBoard = Board(0, 0) E []

