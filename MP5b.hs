module MP5b where

import Data.Maybe
import Data.Ord
import Data.List
import Data.Tree
import Data.Map (Map, empty, fromList, (!), findWithDefault, member, insert, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Concurrent
import Control.Monad.State
import System.IO
import System.Console.ANSI
import GHC.IO
import Debug.Trace
import Text.Read (readMaybe)


-- one player has B, the other W;  pretty-pretting happens only for the
-- board though
data Piece = B | W deriving (Eq, Show)


opponent :: Piece -> Piece
opponent B = W
opponent W = B


-- function to count the number of pieces on the board given W/B
countPieces :: Board -> Piece -> Int
countPieces (Board _ pieces) piece = length $ filter (\(_, _, p) -> p == piece) pieces


-- coordinates go from bottom left to top right
data Board = Board (Int, Int) [(Int, Int, Piece)] deriving (Eq)


-- it's always the last piece played which when it's the current
-- player's turn
turn :: Board -> Piece
turn (Board _ [(_, _, piece)]) = opponent piece


-- the board is full if all fields have a piece on it
full :: Board -> Bool
full (Board (w, h) pieces) = length pieces == w * h


-- to determine whether a calculated position is in bounds of the board
validPosition :: (Int, Int) -> (Int, Int) -> Bool
validPosition (w, h) (x, y) =
  x >= 0 && x < w && y >= 0 && y < h


-- generate set of coordinates that form a line in the given direction
ray :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
ray (w, h) (x, y) (dx, dy) =
  takeWhile (validPosition (w, h)) $ zip (tail (iterate (dx+) x)) (tail (iterate (dy+) y))


-- all deltas into every direction possible
directions :: [(Int, Int)]
directions = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]


-- return the piece at a certain coordinate, if any
at :: [(Int, Int, Piece)] -> (Int, Int) -> Maybe (Int, Int, Piece)
at pieces (x, y) =
  find (\(x', y', _) -> x' == x && y' == y) pieces


playDirection :: Board -> (Int, Int) -> (Int, Int) -> Piece -> [(Int, Int, Piece)]
playDirection (Board dimensions pieces) direction (x, y) player =
  case foldl (\(state, xs) maybePiece -> case maybePiece of
                                           Just (x, y, piece) -> case state of
                                                                   Start -> if piece == opponent player then (FoundOpposite, (x, y, player) : xs) else (Abort, [])
                                                                   FoundOpposite -> if piece == player then (FoundSame, (x, y, player) : xs) else (FoundOpposite, (x, y, player) : xs)
                                                                   FoundSame -> (FoundSame, xs)
                                                                   Abort -> (Abort, [])
                                           Nothing -> if state == FoundSame then (FoundSame, xs) else (Abort, [])) (Start, []) (map (at pieces) (ray dimensions (x, y) direction)) of
    (FoundSame, xs) -> xs
    _ -> []


-- play a known legal move by adding the piece to the board and applying
-- it's effect, if any, to the board
playMove :: Board -> (Int, Int) -> Piece -> Board
playMove board@(Board dimensions pieces) (x, y) piece =
  let partial = (x, y, piece):concat (map (\direction -> playDirection board direction (x, y) piece) directions)
      rest = filter (\(x, y, _) -> at partial (x, y) == Nothing) pieces
  in
    Board dimensions (partial ++ rest)


data SearchState = Start | FoundOpposite | FoundSame | Abort deriving (Eq)


-- generate all possible moves for one of the players and return the new
-- board with the move already applied; as always, the last move will be
-- at the top of the piece list
possibleMoves :: Board -> Piece -> [Board]
possibleMoves board@(Board dimensions@(w, h) pieces) player =
  -- all empty places
  -- only if a neighbouring piece is of the opposite colour
  -- only if there's piece of the same colour somewhere after the neighbouring piece
  let empties = filter (\piece -> at pieces piece == Nothing) [(x, y) | x <- [0..w-1], y <- [0..h-1]]
      moves = filter (\piece -> any id (map (\direction -> foldl (\state maybePiece -> case maybePiece of
                                                                                         Just (x, y, piece) -> case state of
                                                                                                                 Start -> if piece == opponent player then FoundOpposite else Abort
                                                                                                                 FoundOpposite -> if piece == player then FoundSame else FoundOpposite
                                                                                                                 FoundSame -> FoundSame
                                                                                                                 Abort -> Abort
                                                                                         Nothing -> if state == FoundSame then FoundSame else Abort) Start (map (at pieces) (ray dimensions piece direction)) == FoundSame) directions)) empties
  in
    map (\move -> playMove board move player) moves


-- just making sure we can print boards readably
instance Show Board where
  show (Board (w, h) path) =
    concat (intersperse "\n" ((map (\(line, y) -> line ++ " " ++ (show (h-y-1))) (zip (map (\y -> concat (intersperse " " (map (\x -> case find (\(x', y', _) -> x' == x && y' == h-y-1) path of
                                                                                                                                        Just (_, _, B) -> if (x, h-y-1, B) == (head path) then "B" else "b"
                                                                                                                                        Just (_, _, W) -> if (x, h-y-1, W) == (head path) then "W" else "w"
                                                                                                                                        Nothing -> "_") [0..w-1]))) [0..h-1]) [0..h-1])) ++
                              [(concat (intersperse " " (map show [0..w-1])))]))


prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune _ (Node x []) = Node x []
prune n (Node x ns) = Node x $ map (prune (n-1)) ns


data ScoreValue = Won | Lost | Moves Int deriving (Eq, Show)


instance Ord ScoreValue where
  compare Won Won = EQ
  compare Lost Lost = EQ
  compare Won _ = GT
  compare Lost _ = LT
  compare _ Won = LT
  compare _ Lost = GT
  compare (Moves x) (Moves y) = compare x y


data Scored a = Scored { score :: ScoreValue, scoredValue :: a }


instance Eq (Scored a) where
  (Scored x _) == (Scored y _) = x == y


instance Ord (Scored a) where
  compare (Scored x _) (Scored y _) = compare x y


instance Show a => Show (Scored a) where
  show (Scored s v) = "Moves: " ++ show s ++ "\n\n" ++ show v


-- Minimax function from lecture notes
minimax :: (a -> Scored a) -> Tree a -> Scored a
minimax scorefn (Node _ ns) = maximize ns
  where maximize = maximumBy (comparing score) . map (eval minimize)
        minimize = minimumBy (comparing score) . map (eval maximize)
        eval _ (Node x []) = scorefn x
        eval f (Node x ns) = let Scored s _ = f ns in Scored s x


canMove :: Board -> Piece -> Bool
canMove board player = possibleMoves board player /= []


numberOfMoves :: Board -> Piece -> Scored Board
numberOfMoves board player | not (canMove board player) = Scored Lost board
                        | not (canMove board (opponent player)) = Scored Won board
                        | otherwise = Scored (Moves (length (possibleMoves board player))) board


gameTree :: Board -> Tree Board
gameTree board@(Board _ ((_, _, piece):_)) = Node board
  $ map gameTree 
  $ possibleMoves board (opponent piece)


-- convenience function for printing a tree
-- e.g., printTree $ playMoves [1..6]
printTree :: Board -> IO ()
printTree = putStrLn . drawTree . fmap show . gameTree


-- read a pair of integer coordinates and retry if the format was
-- invalid or the move itself wasn't valid on the board for the given
-- player
readMove :: Board -> Piece -> IO Board
readMove board piece = do
  line <- getLine
  case readMaybe ("(" ++ line ++ ")") of
    Just coordinate -> let newBoard = playMove board coordinate piece in
      if find (\move -> move == newBoard) (possibleMoves board piece) == Nothing
      then do
        putStrLn "Invalid move, try again?"
        hFlush stdout
        readMove board piece
      else do
        return newBoard
    Nothing -> do
      putStrLn "Couldn't read coordinate, try again?"
      hFlush stdout
      readMove board piece


-- Plays a game with the AI
playAI :: Int -> IO (Maybe Piece)
playAI depth = let board = Board (8, 8) [(3, 3, B), (4, 4, B), (3, 4, W), (4, 3, W)] in do
    print board
    play board B
  where 
    play board player 
        | not (canMove board player) || full board = do
            let blackScore = countPieces board B
            let whiteScore = countPieces board W            
            putStrLn ("\nBlack score: " ++ show blackScore)
            putStrLn ("White score: " ++ show whiteScore)
            if blackScore > whiteScore then do
                putStrLn "Black wins!\n--------------------"
                return (Just B)
            else if whiteScore > blackScore then do
                putStrLn "White wins!\n--------------------"
                return (Just W)
            else do
                putStrLn "It's a draw!\n--------------------"
                return Nothing
        | player == B = do
            putStr "\n[Black Move]\nEnter a move as 'X,Y': "
            hFlush stdout
            newBoard <- readMove board B
            let scored = minimax (\board -> numberOfMoves board B) (prune depth (gameTree board))
                moves' = score scored
            putStrLn ("\n" ++ "Black score: " ++ show (countPieces board B) ++ "\nWhite " ++ show (countPieces board W) ++ "\n--------------------\n\n\n\n\n[W Possible " ++ show moves' ++ "]\nPlayer Black moved\n--------------------")
            print newBoard
            play newBoard W
        | player == W = do
            let scored = minimax (\board -> numberOfMoves board W) (prune depth (gameTree board))
                moves' = score scored
                newBoard = scoredValue scored
            putStrLn ("\n" ++ "Black score: " ++ show (countPieces board B) ++ "\nWhite " ++ show (countPieces board W) ++ "\n--------------------\n\n\n\n\n[B Possible " ++ show moves' ++ "]\nComputer White moved\n--------------------")
            print newBoard
            play newBoard B


-- let the AI play against itself, return winner, if any
playAI2 :: Int -> IO (Maybe Piece)
playAI2 depth = let board = Board (8, 8) [(3, 3, W), (4, 4, W), (3, 4, B), (4, 3, B)] in do
    print board
    play board B
  where 
    play board player 
        | not (canMove board player) || full board = do
            let blackScore = countPieces board B
            let whiteScore = countPieces board W            
            putStrLn ("\nBlack score: " ++ show blackScore)
            putStrLn ("White score: " ++ show whiteScore)
            if blackScore > whiteScore then do
                putStrLn "Black wins!\n--------------------"
                return (Just B)
            else if whiteScore > blackScore then do
                putStrLn "White wins!\n--------------------"
                return (Just W)
            else do
                putStrLn "It's a draw!\n--------------------"
                return Nothing
        | player == B = do
            let scored = minimax (\board -> numberOfMoves board B) (prune depth (gameTree board))
                moves' = score scored
                newBoard = scoredValue scored
            putStrLn ("\n" ++ "Black score: " ++ show (countPieces board B) ++ "\nWhite " ++ show (countPieces board W) ++ "\n--------------------\n\n\n\n\n[W Possible " ++ show moves' ++ "]\nComputer Black moved\n--------------------")
            print newBoard
            play newBoard W
        | player == W = do
            let scored = minimax (\board -> numberOfMoves board W) (prune depth (gameTree board))
                moves' = score scored
                newBoard = scoredValue scored
            putStrLn ("\n" ++ "Black score: " ++ show (countPieces board B) ++ "\nWhite " ++ show (countPieces board W) ++ "\n--------------------\n\n\n\n\n[B Possible " ++ show moves' ++ "]\nComputer White moved\n--------------------")
            print newBoard
            play newBoard B
