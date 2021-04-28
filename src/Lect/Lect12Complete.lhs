% CS 340: Programming Paradigms and Patterns
% Lect 12 - Search
% Michael Lee

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
-- note that we use the \begin{code} and \end{code} markers in this literate
-- source file instead of '>' line prefixes --- this is because there's going 
-- to be a bit more code than usual!

module Lect.Lect12Complete where
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

We'll explore a few problems that are natural candidates for search-based
solutions, starting with maze-solving. As a fun programming exercise, we'll
start by considering how to generate random mazes of arbitrary size.


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
openWall l1 l2 mz@(Maze _ _ cmap) = 
  mz { mazeAdjMap = insertWith (++) l2 [l1] $ insertWith (++) l1 [l2] cmap }
\end{code}




-- Random values and State

Before we think about generating random mazes, we need to think about how to
generate random numbers. How would an API for obtaining random numbers look?

How's this API for a function that takes an input N and returns a random number
in the range [0,N)?

    randomRange :: Int -> Int


This can't possibly work! Purity requires that a function always return the same
result for given input(s). So there's just really no way of writing this
function. 

The only way for a function to return "random" values is by being stateful ---
i.e., by being passed some state to use as the basis for returning a random
number alongside an updated state. This is the basis for pseudo-random number
generators (PRNGs). In this context, we often to the state as a "seed" value.

A simple way of updating seed values is the Lehmer RNG algorithm:

    seed' = a*seed `mod` p

where `p` is a prime number (we won't worry about how to pick `a`).

---

Here's a version of `randomRange` that works on this principle. We define the
`Seed` type to differentiate the seed value from the range. We rely on `mod` to
help us compute a pseudo-random value based on the input seed that is in the
specified range, too.

\begin{code}
type Seed = Int

randomRange :: Int -> Seed -> (Int, Seed)
randomRange max seed = (seed `mod` max, 7*seed `mod` 101)
\end{code}


To generate a series of random numbers, we need to chain together calls to
`randomRange`, passing along the new seeds from call to call:

\begin{code}
fourRands :: Int -> Seed -> [Int]
fourRands max s0 = let (v1, s1) = randomRange max s0
                       (v2, s2) = randomRange max s1
                       (v3, s3) = randomRange max s2
                       (v4, _)  = randomRange max s3
                   in [v1, v2, v3, v4]
\end{code}


This is a natural fit for the State monad. We can use a State monad to carry
along and update the seed in order to help us extract new random values when 
needed:

\begin{code}
getRandomRange :: Int -> State Seed Int
getRandomRange max = do s <- get
                        let (x, s') = randomRange max s
                        put s'
                        return x


nRands :: Int -> Int -> State Seed [Int]
nRands 0 _ = return []
nRands n max = (:) <$> getRandomRange max <*> nRands (n-1) max

-- or, using the higher order applicative function replicateM
nRands' n max = replicateM n $ getRandomRange max
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
lists using a StdGen --- the PRNG state --- pulled out of a State monad:

\begin{code}
getRandom :: (Int, Int) -> State StdGen Int
getRandom range = do gen <- get
                     let (val, gen') = randomR range gen
                     put gen'
                     return val

getShuffled :: [a] -> State StdGen [a]
getShuffled l = do gen <- get
                   let (g1, g2) = split gen
                       l' = shuffle' l (length l) g1
                   put g2
                   return l'
\end{code}

---

And now we're ready to implement a random maze generator!

We're going to use a maze generation algorithm known as *reursive backtracking*,
which starts with a fully "disconnected" maze (i.e., no cells reachable from any
other), and proceeds as follows:

  1. Pick a starting cell to visit -- this is the "current" cell.

  2. Pick a neighboring cell that has yet to be visited and connect it to the
     current cell. The neighboring cell becomes the new current cell.

  3. Keep repeating (2) until the current cell's neighbors have all been
     visited, then back up to the previous cell and continue. 

  4. All cells should be connected when we back out of the starting cell.


\begin{code}
-- attempt 1: create a maze with a single "tunnel"
genMazeSimple :: MazeDims -> State StdGen Maze
genMazeSimple dims = gen (Maze dims [] empty) (1, 1)
  where gen :: Maze -> MazeLoc -> State StdGen Maze
        gen mz@(Maze _ _ cmap) currLoc = do
          nLocs <- getShuffled $ adjLocs dims currLoc
          let (nLoc:_) = nLocs
          if nLoc `member` cmap 
          then return mz
          else gen (openWall currLoc nLoc mz) nLoc


-- maze generator using recursive backtracking
genMaze :: MazeDims -> State StdGen Maze
genMaze dims = gen (Maze dims [] empty) (1, 1)
  where gen :: Maze -> MazeLoc -> State StdGen Maze
        gen mz currLoc = do
          nLocs <- getShuffled $ adjLocs dims currLoc
          foldM (\mz'@(Maze _ _ cmap) nLoc ->
                  if nLoc `member` cmap
                  then return mz' 
                  else gen (openWall currLoc nLoc mz') nLoc)
                mz nLocs


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


Searching through nodes of type `a` relies on these functions:

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
search goal adj comb unvisited visited
  | null unvisited = Nothing
  | goal (head unvisited) = Just (head unvisited)
  | otherwise = let (n:ns) = unvisited
                in debug n $ 
                   search goal adj comb
                          (comb (removeDups (adj n)) ns)
                          (n:visited)
  where removeDups = filter (not . (`elem` (unvisited ++ visited)))

debug :: Show a => a -> b -> b
debug x y = unsafePerformIO clearScreen `seq`
            unsafePerformIO (setCursorPosition 0 0) `seq`
            unsafePerformIO (putStrLn $ show x) `seq`
            unsafePerformIO (threadDelay $ 3*10^5) `seq`
            y
\end{code}


-- Uninformed search

Uninformed seach strategies (aka "brute force" strategies) are goal-agnostic.
I.e., they are uninformed by the search domain. They may, however, impose an
order on when nodes are visited based on other factors.

Depth-first search considers newly discovered nodes before previously unvisited
ones, while breadth-first search considers nodes in FIFO order of discovery.
These are easy to express in terms of our higher-order search pattern.

\begin{code}
dfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
dfs goal succ start = search goal succ (++) [start] []

bfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
bfs goal succ start = search goal succ (flip (++)) [start] []
\end{code}


Let's solve our maze using uninformed search:

\begin{code}
solveMaze :: Maze -> Maybe Maze
solveMaze mz@(Maze (w,h) _ _) =
  let entry = (1, 1) -- enter at top left
      exit = (w, h)  -- exit at bottom right
  in dfs ((== exit) . head . mazePath)
         nextPaths
         (mz { mazePath = [entry]})

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
bestFirstSearch goal succ cost start = search goal succ comb [start] []
  where comb new old = sortOn cost (new ++ old)
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
bfsSolveMaze = solveMaze' (length . mazePath)
\end{code}

Note its behavioral resemblance to breadth-first search!


-- Informed Search

Informed search strategies rely on domain knowledge -- typically related to the
search goal -- to decide which node to visit next. 

In our maze solver, we can rank paths based on whether they appear to get us
closer to the exit. Since we don't know the actual route to the exit (that's
what we're searching for!) we can only guess at it.

Let's implement a maze solver, again based on best-first search, that uses an
estimate of the remaining distance to the exit as a cost function:

\begin{code}
bfsSolveMaze' :: Maze -> Maybe Maze
bfsSolveMaze' mz@(Maze (w,h) _ _) = solveMaze' cost mz
  where cost mz'@(Maze _ p@((x,y):_) _) = abs (w-x) + abs (y-h)
\end{code}

The strategy above is *greedy*. It chooses the next option based on immediate
benefit without necessarily considering the big picture. Consider how it
performs on the following maze:

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

A better strategy is to factor both the distance traversed so far *and* the
estimated remaining distance into the cost function. We call a best-first search
using such a cost function A* (read "A-star") search.

When our estimate of the remaining distance never overshoots the actual cost of
reaching the goal, A* search is guaranteed to return the lowest-cost path. We
call such an estimation function an *admissible heuristic*.

Let's implement an A* search solver for our maze:

\begin{code}
aStarSolveMaze :: Maze -> Maybe Maze
aStarSolveMaze mz@(Maze (w,h) _ _) = solveMaze' cost mz
  where cost mz'@(Maze _ p@((x,y):_) _) = abs (w-x) + abs (y-h) + length p
\end{code}


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
