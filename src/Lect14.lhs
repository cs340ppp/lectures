\begin{code}
{-# LANGUAGE FlexibleInstances #-}

module Lect14 where
import Prelude hiding (lines)
import Data.Maybe
import Data.Ord
import Data.List hiding (lines)
import Data.List.Split (chunksOf)
import Data.Tree
import Data.Map (Map, empty, fromList, keys, elems, assocs,
                 findWithDefault, member, insert, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Monad.State
import System.IO
import System.Console.ANSI
import Control.Concurrent
import GHC.IO
\end{code}


Search
======

Agenda:
  - Adversarial search
    - Tic-Tac-Toe
    - Minimax


Adversarial search
------------------

There are problems where it doesn't make sense to apply the same search strategy
at each step. For example, consider moves in a two-person adversarial game as
nodes in a search space, with the search goal of identifying how to get to the
winning move. It makes no sense to assume that both players are going to carry
out strategies that helps only one of the players win!


-- Tic-Tac-Toe

Let's model tic-tac-toe as an adversarial game for demonstrating this search
problem. Here are some tic-tac-toe types and functions:

\begin{code}
data Piece = X | O deriving (Eq, Show, Read)

type Board = [Maybe Piece]

instance {-# OVERLAPPING #-} Show Board where
  show = intercalate "\n" . chunksOf 3 . concat . map showSquare
    where showSquare Nothing = "-"
          showSquare (Just p) = show p

opponent :: Piece -> Piece
opponent X = O
opponent O = X

emptyBoard :: Board
emptyBoard = replicate 9 Nothing

-- play a piece at the specified position (valid positions = 1-9)
playMove :: Int -> Piece -> Board -> Board
playMove n p b = let (pre,_:post) = splitAt (n-1) b
               in pre ++ [Just p] ++ post

-- play all given moves starting with an empty board
playMoves :: [Int] -> Board
playMoves moves = play moves X emptyBoard
  where play [] _ b = b
        play (m:ms) p b = play ms (opponent p) $ playMove m p b 

-- get back the rows/cols/diags as separate lists
lines :: Board -> [[Maybe Piece]]
lines b = let dim = 3
              rows = chunksOf dim b
              cols = transpose rows
              diags = [[rs !! i !! i | i <- [0..(dim-1)]] 
                       | rs <- [rows, reverse rows]]
          in concat [rows, cols, diags]

-- did the specified piece win?
wins :: Piece -> Board -> Bool
wins p = any (all (== Just p)) . lines

-- did someone win?
won :: Board -> Bool
won b = wins X b || wins O b

-- is the board full?
full :: Board -> Bool
full = all (/= Nothing)

-- whose turn is it?
turn :: Board -> Piece
turn b = let xs = length $ filter (== Just X) b
             os = length $ filter (== Just O) b
         in if xs == os then X else O
\end{code}


We can use the above to build an interactive game:

\begin{code}
playInteractive :: IO ()
playInteractive = play X emptyBoard 
  where play turn board
          | wins X board = putStrLn "X wins!"
          | wins O board = putStrLn "O wins!"
          | full board = putStrLn "Drawn"
          | otherwise = do
              putStr "Enter a move: "
              move <- readLn
              case board !! (move-1) of
                (Just _) -> do putStrLn "Illegal move"
                               play turn board
                _ -> do let board' = playMove move turn board
                        print board'
                        play (opponent turn) board'
\end{code}

---

In order to model the game as a search problem, we can create a "game tree",
where each node in the tree represents the state of the board, and its children
represent the different board states resulting from playing a valid move on the
parent's board. Let's define the needed functions:

\begin{code}
gameTree :: Board -> Tree Board
gameTree b = Node b 
             $ map gameTree 
             $ map (\i -> playMove (i+1) (turn b) b) moves
  where moves | won b || full b = []
              | otherwise = findIndices (== Nothing) b

-- constrains the maximum height of a tree
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune _ (Node x []) = Node x []
prune n (Node x ns) = Node x $ map (prune (n-1)) ns

-- convenience function for printing a tree
-- e.g., printTree $ playMoves [1..6]
printTree :: Board -> IO ()
printTree = putStrLn . drawTree . fmap show . gameTree
\end{code}


Next, to help us compare board states corresponding to nodes in our search
space, we can compute and attach scores to board values. Here's a data type to
make doing that easier:

\begin{code}
data Scored a = Scored { score :: Int, scoredVal :: a }

instance Eq (Scored a) where
  (Scored x _) == (Scored y _) = x == y

instance Ord (Scored a) where
  compare (Scored x _) (Scored y _) = compare x y

instance Show a => Show (Scored a) where
  show (Scored s v) = "Score: " ++ show s ++ "\n\n" ++ show v
\end{code}


Let's write a function to score a board from the perspective of a given player.
Instead of trying to predict whether a game in progress will lead to a win or
loss (that's the job of our search algorithm!), we only assign non-zero scores
to boards that have already been decided:

\begin{code}
scoreBoard :: Piece -> Board -> Scored Board
scoreBoard p b | wins p b = Scored 100 b
               | wins (opponent p) b = Scored (-100) b
               | otherwise = Scored 0 b

-- convenience function for printing a scored tree
-- e.g., printScoredTree $ playMoves [1,5,9,2,8,7]
printScoredTree :: Board -> IO ()
printScoredTree = putStrLn . drawTree . fmap (show . scoreBoard X) . gameTree
\end{code}

Note how our tree only contains non-zero scores for leaves. For this reason, it
makes sense to only compute scores during our search when we reach a leaf node.


-- Minimax

Finally, we can frame the search problem: what is the best possible outcome for
a given player and a starting board, and what move should we take to get there?

The critical observation is that our scores are computed from the point of view
of a single player, while there are two opposing players in the game. Given
players X and O with the game tree being scored from X's perspective, X will
strive to maximize the final score during their turn, while O will strive to
minimize the score in the following turn; this will alternate throughout our
search. 

The minimax algorithm carries out this logic. 

\begin{code}
minimax :: (a -> Scored a) -> Tree a -> Scored a
minimax scorefn (Node _ ns) = maximize ns
  where maximize = maximum . map (eval minimize)
        minimize = minimum . map (eval maximize)
        eval _ (Node x []) = scorefn x
        eval f (Node x ns) = f ns
\end{code}


However, the above version only locates the final board, and fails to give us
the next best move along the path to that board. This next version propagates
the score of the final board back up the tree so we can select the next move:

\begin{code}
minimax' :: (a -> Scored a) -> Tree a -> Scored a
minimax' scorefn (Node _ ns) = maximize ns
  where maximize = maximumBy (comparing score) . map (eval minimize)
        minimize = minimumBy (comparing score) . map (eval maximize)
        eval _ (Node x []) = scorefn x
        eval f (Node x ns) = let Scored s _ = f ns in Scored s x
\end{code}

And now we can write a tic-tac-toe AI driven by our search implementation:

\begin{code}
playAI :: IO ()
playAI = play X emptyBoard
  where play _ b | wins X b = putStrLn "X wins!"
                 | wins O b = putStrLn "O wins!"
                 | full b = putStrLn "Draw"
        play X b = do 
          putStr "Enter a move: "
          move <- readLn
          case b !! (move-1) of
            (Just _) -> do putStrLn "Illegal move"
                           play X b
            _ -> do let b' = playMove move X b
                    print b'
                    play O b'
        play O b = do
          let b' = scoredVal $ minimax' (scoreBoard O) (gameTree b)
          print b'
          play X b'
\end{code}

---

In practice, there are subtrees that we needn't evaluate when we've already
discovered alternate moves that cannot be improved upon (by either player).
"Alpha-beta pruning" is a method for reducing the number of nodes we need to
consider in a minimax search:

\begin{code}
instance Bounded (Scored a) where
  minBound = Scored minBound undefined
  maxBound = Scored maxBound undefined

minimax_ab :: (a -> Scored a) -> Tree a -> Scored a
minimax_ab scorefn (Node _ ns) = maximize minBound maxBound ns
  where maximize a b [] = minBound
        maximize a b (n:ns) | a > b = a
                            | otherwise = let sn = eval a b minimize n
                                              a' = max a sn
                                          in max sn $ maximize a' b ns
        minimize a b [] = maxBound
        minimize a b (n:ns) | a > b = b
                            | otherwise = let sn = eval a b maximize n
                                              b' = min b sn
                                          in min sn $ minimize a b' ns
        eval a b f (Node x []) = scorefn x
        eval a b f (Node x ns) = let Scored s _ = f a b ns
                                 in Scored s x
\end{code}
