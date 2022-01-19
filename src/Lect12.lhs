% CS 340: Programming Paradigms and Patterns
% Lect 12 - Search
% Michael Lee

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
-- note that we use the \begin{code} and \end{code} markers in this literate
-- source file instead of '>' line prefixes --- this is because there's going 
-- to be a bit more code than usual!

module Lect12 where
import Prelude hiding (lines)
import Data.Maybe
import Data.Ord
import Data.List hiding (lines)
import Data.List.Split (chunksOf)
import Data.Tree
import Data.Map (Map, empty, fromList, (!), keys, elems, assocs,
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
  - Why and How?
  - Maze-building
    - Random values and State
  - Search
    - Uninformed search
    - Informed search
  - Adversarial search
    - Tic-Tac-Toe
    - Minimax


Why and How?
------------

Search is one of the most common programming tasks you'll perform. It's
useful to recognize some common search patterns and techniques, and understand
how they might be applied.


Maze-building
-------------

We need some data types to represent mazes:

\begin{code}
type MazeDims = (Int, Int) -- (width,height)

type MazeLoc = (Int, Int) -- (x,y); (1,1) @ top-left

type MazePath = [MazeLoc] -- path through the maze

data Maze = Maze { 
              mazeDims :: MazeDims, 
              mazePath :: MazePath,
              mazeAdjMap :: Map MazeLoc [MazeLoc] 
            } deriving (Eq)

\end{code}


And to help us visualize the mazes we generate, some drawing functions (which
return ASCII art strings):

\begin{code}
-- draw the north and west borders (either lines for disconnected neighbors
-- or spaces for connected ones) of the specified location; we also draw
-- breadcrumbs for the specified path
drawWallsNW :: MazeLoc -> Maze -> (String, String)
drawWallsNW c@(x, y) (Maze (w, h) p cmap) = 
  let nc = (x, y-1)
      wc = (x-1, y)
      adj = findWithDefault [] c cmap
  in ("+" ++ if nc `elem` adj then "   " else "---",
      (if wc `elem` adj then " " else "|") 
        ++ (if c `elem` p then " o " else "   "))

-- draw the entire maze by assembling the location strings
drawMaze :: Maze -> String
drawMaze m@(Maze (w, h) _ _) = (concat $ map drawRow $ chunksOf w drawnCells) 
                                 ++ bot
  where drawRow cs = let (l1, l2) = unzip cs
                     in concat l1 ++ "+\n" ++ concat l2 ++ "|\n"
        drawnCells = [drawWallsNW (x, y) m | y <- [1..h], x <- [1..w]]
        bot = (concat $ replicate w "+---") ++ "+"

instance Show Maze where
  show = drawMaze        
\end{code}


Next, some utility functions for building up our mazes:

\begin{code}
-- return adjacent (but not necessarily accessible) locations
adjLocs :: MazeDims -> MazeLoc -> [MazeLoc]
adjLocs (w, h) (x, y) = 
  [(x', y') | (dx, dy) <- [(-1,0), (0,-1), (1,0), (0,1)],
              let (x', y') = (x+dx, y+dy),
              x' > 0 && x' <= w,
              y' > 0 && y' <= h]

-- connects two adjacent locations in the maze by inserting them into
-- each others' lists in the adjacency map
openWall :: MazeLoc -> MazeLoc -> Maze -> Maze
openWall l1 l2 mz@(Maze _ _ cmap) = undefined
\end{code}


-- Random values and State

Before we think about generating random mazes, we need to think about how to
generate random numbers. How would an API for obtaining random numbers look?

How's this API for a function that takes an input N and returns a random number
in the range [0,N)?

    randomRange :: Int -> Int

---

<Discuss> PRNGs requiring state and "seed" values

A simple way of updating seed values is the Lehmer RNG algorithm:

    seed' = a*seed `mod` p

where `p` is a prime number (we won't worry about how to pick `a`).

---

Let's write a version of `randomRange` that works on this principle:

\begin{code}
type Seed = Int

randomRange :: Int -> Seed -> (Int, Seed)
randomRange max seed = undefined
\end{code}


We can chain together calls to generate a series of random values:

\begin{code}
fourRands :: Int -> Seed -> [Int]
fourRands max s0 = let (v1, s1) = randomRange max s0
                       (v2, s2) = randomRange max s1
                       (v3, s3) = randomRange max s2
                       (v4, _)  = randomRange max s3
                   in [v1, v2, v3, v4]
\end{code}


Let's use the state monad instead:

\begin{code}
getRandomRange :: Int -> State Seed Int
getRandomRange max = undefined


nRands :: Int -> Int -> State Seed [Int]
nRands = undefined
\end{code}


This is how the built-in random number library (System.Random) works. Here are
some relevant classes, instances, and functions:

    class Random a where
      randomR :: RandomGen g => (a, a) -> g -> (a, g)
      random :: RandomGen g => g -> (a, g)
      randomRs :: RandomGen g => (a, a) -> g -> [a]
      randoms :: RandomGen g => g -> [a]

    shuffle' :: RandomGen gen => [a] -> Int -> gen -> [a]

    instance Random Integer
    instance Random Int
    instance Random Double
    instance Random Char
    instance Random Bool

    class RandomGen g where
      next :: g -> (Int, g)
      genRange :: g -> (Int, Int)
      split :: g -> (g, g)

    instance RandomGen StdGen

    mkStdGen :: Int -> StdGen
    newStdGen :: IO StdGen


Let's write some functions that allow us to generate random values and shuffle
lists using a StdGen pulled out of a State monad:

\begin{code}
getRandom :: (Int, Int) -> State StdGen Int
getRandom range = undefined

getShuffled :: [a] -> State StdGen [a]
getShuffled l = undefined
\end{code}

---

And now we're ready to implement a random maze generator!

<Discuss> "Recursive backtracking" algorithm:

  1. Pick a starting cell to visit -- this is the "current" cell.

  2. Pick a neighboring cell that has yet to be visited and connect it to the
     current cell. The neighboring cell becomes the new current cell.

  3. Keep repeating (2) until the current cell's neighbors have all been
     visited, then back up to the previous cell and continue. 

  4. All cells should be connected when we back out of the starting cell.


\begin{code}
-- attempt 1: create a maze with a single "tunnel"
genMazeSimple :: MazeDims -> State StdGen Maze
genMazeSimple dims = undefined


-- maze generator using recursive backtracking
genMaze :: MazeDims -> State StdGen Maze
genMaze dims = undefined


-- convenience function for creating a random maze from the global RNG
randomMaze :: MazeDims -> IO Maze
randomMaze dims = evalState (genMaze dims) <$> newStdGen
\end{code}


Search
------

The search problem can be expressed in terms of a search space made up of a
network of interconnected nodes. A search algorithm seeks to locate a goal node
-- i.e., one satisfying certain criteria -- by traversing this network.

A search strategy should avoid re-visiting nodes, and address the following:

  - if we have multiple nodes under consideration, which do we visit first? 

  - if we discover additional traversable nodes, how do we combine those with
    existing, as yet unvisited nodes?


<Discuss> Search helper functions (nodes of type `a`):

  goal :: a -> Bool
  
    - is a given node the goal node?

  adj :: a -> [a]

    - what node(s) are adjacent to / reachable from the given one?

  comb :: [a] -> [a] -> [a]

    - how do we combine newly discovered nodes with other unvisited ones?


\begin{code}
-- generalized, higher-order search
search :: (Eq a, Show a) => 
          (a -> Bool) 
          -> (a -> [a]) 
          -> ([a] -> [a] -> [a])
          -> [a] -> [a] 
          -> Maybe a
search goal adj comb unvisited visited = undefined

-- convenience function for tracing search execution
debug :: Show a => a -> b -> b
debug x y = unsafePerformIO clearScreen `seq`
            unsafePerformIO (setCursorPosition 0 0) `seq`
            unsafePerformIO (putStrLn $ show x) `seq`
            unsafePerformIO (threadDelay $ 3*10^5) `seq`
            y
\end{code}


-- Uninformed search

<Discuss> Uninformed / "brute force" search strategies

<Discuss> Depth-first (LIFO) & Breadth-first search (FIFO)

\begin{code}
dfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
dfs goal succ start = undefined

bfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
bfs goal succ start = undefined
\end{code}


Let's solve our maze using uninformed search:

\begin{code}
solveMaze :: Maze -> Maybe Maze
solveMaze mz@(Maze (w,h) _ _) = undefined

-- given a maze with a non-empty path, return a list of mazes, each of
-- which extends the path by one location (based on the adjacency map)
nextPaths :: Maze -> [Maze]
nextPaths mz@(Maze dims p@(loc:_) cmap) = do
  adj <- filter (not . flip elem p) $ findWithDefault [] loc cmap
  return $ mz { mazePath = adj:p }

-- maze for testing nextPaths
testMaze1 = openWall (1,1) (2,1) 
            $ openWall (1,1) (1,2)
            $ openWall (1,2) (2,2)
            $ Maze (2,2) [(1,1)] empty

-- convenience function for solving a maze given dimensions and a solver
solveRandomMaze :: MazeDims -> (Maze -> Maybe Maze) -> IO Maze
solveRandomMaze dims solver = do mz <- randomMaze dims
                                 return $ fromJust $ solver mz
\end{code}

---

Besides order of discovery, we can also rank unvisited nodes using some
cost function:

  cost :: Ord b => a -> b

    - returns an orderable value representing the cost for a node of type `a`

When considering multiple unvisited nodes, the "best-first" search strategy
chooses the node with lowest cost:

\begin{code}
bestFirstSearch :: (Eq a, Show a, Ord b) => 
                   (a -> Bool) 
                   -> (a -> [a])
                   -> (a -> b) 
                   -> a -> Maybe a
bestFirstSearch goal succ cost start = undefined
\end{code}


Here's an updated `solveMaze'` driven by best-first search, which takes a cost
function expressed in terms of mazes.

\begin{code}
solveMaze' :: Ord a => (Maze -> a) -> Maze -> Maybe Maze
solveMaze' cost mz@(Maze (w,h) _ _) =  
    let entry = (1,1)
        exit = (w, h)
    in bestFirstSearch ((==exit) . head . mazePath) 
                       nextPaths
                       cost
                       (mz { mazePath = [entry]})
\end{code}


A simple cost function is one that returns the length of the path through the
maze thus far. Let's write it:

\begin{code}
bfsSolveMaze :: Maze -> Maybe Maze
bfsSolveMaze = undefined
\end{code}


-- Informed Search

<Discuss> Informed search strategies

Let's implement a maze solver, again based on best-first search, that uses an
estimate of the remaining distance to the exit as a cost function:

\begin{code}
bfsSolveMaze' :: Maze -> Maybe Maze
bfsSolveMaze' mz@(Maze (w,h) _ _) = undefined
\end{code}

The strategy above is *greedy*. How would it perform on the following maze?

    +---+---+---+---+---+---+---+---+---+---+
    |Entry          |                       |
    +   +---+---+   +   +---+---+---+---+   +
    |   |           |   |       |       |   |
    +   +   +---+---+   +   +   +   +   +   +
    |   |               |   |   |   |   |   |
    +   +---+---+---+---+   +   +   +   +   +
    |   |       |       |   |   |   |   |   |
    +   +   +   +   +   +   +   +   +   +   +
    |   |   |   |   |   |   |   |   |   |   |
    +   +   +   +   +   +   +   +   +   +   +
    |   |   |   |   |   |   |   |   |   |   |
    +   +   +   +   +   +   +   +   +   +   +
    |       |       |       |       |   Exit|
    +---+---+---+---+---+---+---+---+---+---+

\begin{code}
testMaze2 :: Maze
testMaze2 = build locs $ Maze (10,7) [] empty
  where build [l1] mz = mz
        build (l1:l2:ls) mz = build (l2:ls) $ openWall l1 l2 mz
        locs = [(1,1),(2,1),(3,1),(4,1),(4,2),(3,2),(2,2),(2,3),
                (3,3),(4,3),(5,3),(5,2),(5,1),(6,1),(7,1),(8,1),
                (9,1),(10,1),(10,2),(10,3),(10,4),(10,5),(10,6),
                (10,7),(9,7),(9,6),(9,5),(9,4),(9,3),(9,2),(8,2),
                (8,3),(8,4),(8,5),(8,6),(8,7),(7,7),(7,6),(7,5),
                (7,4),(7,3),(7,2),(6,2),(6,3),(6,4),(6,5),(6,6),
                (6,7),(5,7),(5,6),(5,5),(5,4),(4,4),(4,5),(4,6),
                (4,7),(3,7),(3,6),(3,5),(3,4),(2,4),(2,5),(2,6),
                (2,7),(1,7),(1,6),(1,5),(1,4),(1,3),(1,2),(1,1)]
\end{code}

---

<Discuss> A* search

Let's implement an A* search solver for our maze:

\begin{code}
aStarSolveMaze :: Maze -> Maybe Maze
aStarSolveMaze mz@(Maze (w,h) _ _) = undefined
\end{code}


Adversarial search
------------------

<Discuss> Adversarial search

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

<Discuss> Game trees

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


<Discuss> Nodes and scoring

\begin{code}
data Scored a = Scored { score :: Int, scoredVal :: a }

instance Eq (Scored a) where
  (Scored x _) == (Scored y _) = x == y

instance Ord (Scored a) where
  compare (Scored x _) (Scored y _) = compare x y

instance Show a => Show (Scored a) where
  show (Scored s v) = "Score: " ++ show s ++ "\n\n" ++ show v
\end{code}


Let's write a function to score a board from the perspective of a given player:

\begin{code}
scoreBoard :: Piece -> Board -> Scored Board
scoreBoard p b = undefined

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

<Discuss> Minimax algorithm 

\begin{code}
minimax :: (a -> Scored a) -> Tree a -> Scored a
minimax scorefn (Node _ ns) = maximize ns
  where maximize = maximum . map (eval minimize)
        minimize = minimum . map (eval maximize)
        eval _ (Node x []) = scorefn x
        eval f (Node x ns) = f ns
\end{code}


The above version only locates the final board. This next version propagates
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

<Discuss> Alpha-Beta pruning

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
