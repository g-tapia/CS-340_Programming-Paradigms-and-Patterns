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


-- one player has X, the other O;  pretty-pretting happens only for the
-- board though
data Piece = X | O deriving (Eq, Show)


opponent :: Piece -> Piece
opponent X = O
opponent O = X


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
                                                                                                                                        Just (_, _, X) -> if (x, h-y-1, X) == (head path) then "X" else "x"
                                                                                                                                        Just (_, _, O) -> if (x, h-y-1, O) == (head path) then "O" else "o"
                                                                                                                                        Nothing -> "_") [0..w-1]))) [0..h-1]) [0..h-1])) ++
                              [(concat (intersperse " " (map show [0..w-1])))]))


prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune _ (Node x []) = Node x []
prune n (Node x ns) = Node x $ map (prune (n-1)) ns


data ScoreValue = Won | Lost | Score Int deriving (Eq, Show)


instance Ord ScoreValue where
  compare Won Won = EQ
  compare Lost Lost = EQ
  compare Won _ = GT
  compare Lost _ = LT
  compare _ Won = LT
  compare _ Lost = GT
  compare (Score x) (Score y) = compare x y


data Scored a = Scored { score :: ScoreValue, scoredValue :: a }


instance Eq (Scored a) where
  (Scored x _) == (Scored y _) = x == y


instance Ord (Scored a) where
  compare (Scored x _) (Scored y _) = compare x y


instance Show a => Show (Scored a) where
  show (Scored s v) = "Score: " ++ show s ++ "\n\n" ++ show v


-- Minimax function from lecture notes
minimax :: (a -> Scored a) -> Tree a -> Scored a
minimax scorefn (Node _ ns) = maximize ns
  where maximize = maximumBy (comparing score) . map (eval minimize)
        minimize = minimumBy (comparing score) . map (eval maximize)
        eval _ (Node x []) = scorefn x
        eval f (Node x ns) = let Scored s _ = f ns in Scored s x


canMove :: Board -> Piece -> Bool
canMove board player = possibleMoves board player /= []


scoreBoard :: Board -> Piece -> Scored Board
scoreBoard board player | not (canMove board player) = Scored Lost board
                        | not (canMove board (opponent player)) = Scored Won board
                        | otherwise = Scored (Score (length (possibleMoves board player))) board


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
playAI depth = let board = Board (8, 8) [(3, 3, O), (4, 4, O), (3, 4, X), (4, 3, X)] in do
  print board
  play board X
  where play board player | not (canMove board (opponent player)) = do
                              putStrLn ((show player) ++ " wins!")
                              return (Just (opponent player))
                          | full board = do
                              putStrLn "Draw!"
                              return Nothing
        play board X = do
          putStr "Enter a move as 'X,Y': "
          hFlush stdout
          newBoard <- readMove board X
          print newBoard
          play newBoard O
        play board O = do
          let scored = minimax (\board -> scoreBoard board O) (prune depth (gameTree board))
              score' = score scored
              newBoard = scoredValue scored in do
            putStrLn ("Computer moved " ++ show score' ++ ":")
            print newBoard
            play newBoard X


-- let the AI play against itself, return winner, if any
playAI2 :: Int -> IO (Maybe Piece)
playAI2 depth = let board = Board (8, 8) [(3, 3, O), (4, 4, O), (3, 4, X), (4, 3, X)] in do
  print board
  play board X
  where play board player | not (canMove board player) = do
                              putStrLn ((show (opponent player)) ++ " wins!")
                              return (Just (opponent player))
                          | full board = do
                              putStrLn "Draw!"
                              return Nothing
        play board X = do
          let scored = minimax (\board -> scoreBoard board X) (prune depth (gameTree board))
              score' = score scored
              newBoard = scoredValue scored in do
            putStrLn ("Computer X moved " ++ show score' ++ ":")
            print newBoard
            play newBoard O
        play board O = do
          let scored = minimax (\board -> scoreBoard board O) (prune depth (gameTree board))
              score' = score scored
              newBoard = scoredValue scored in do
            putStrLn ("Computer O moved " ++ show score' ++ ":")
            print newBoard
            play newBoard X
