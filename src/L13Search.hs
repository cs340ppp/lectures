{-# LANGUAGE FlexibleInstances #-}

module L13Search where
import Prelude hiding (lines)
import Data.Maybe
import Data.Ord
import Data.List hiding (lines, insert)
import Data.List.Split (chunksOf)
import Data.Tree
import Data.Word
import Data.Map (Map, empty, fromList, (!), keys, elems, assocs,
                 findWithDefault, member, insert, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Monad.State
import System.IO
import System.Console.ANSI
import Control.Concurrent
import GHC.IO

type MazeDims = (Int, Int) -- (width,height)

type MazeLoc = (Int, Int) -- (x,y); (1,1) @ top-left

type MazePath = [MazeLoc] -- path through the maze

data Maze = Maze { 
              mazeDims :: MazeDims, 
              mazePath :: MazePath,
              mazeAdjMap :: Map MazeLoc [MazeLoc] 
            } deriving (Eq)

emptyMaze :: MazeDims -> Maze
emptyMaze d = Maze d [] empty

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

threeRandomInts :: RandomGen g => (Int,Int) -> g -> (Int, Int, Int)
threeRandomInts range g = undefined

getRandom :: (Int, Int) -> State StdGen Int
getRandom range = undefined

threeRandomInts' :: (Int,Int) -> State StdGen (Int, Int, Int)
threeRandomInts' range = undefined


getShuffled :: [a] -> State StdGen [a]
getShuffled l = undefined

-- attempt 1: create a maze with a single "tunnel"
genMazeSimple :: MazeDims -> State StdGen Maze
genMazeSimple dims = undefined

-- maze generator using recursive backtracking
genMaze :: MazeDims -> State StdGen Maze
genMaze dims = undefined

-- convenience function for creating a random maze from the global RNG
randomMaze :: MazeDims -> IO Maze
randomMaze dims = evalState (genMaze dims) <$> newStdGen

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

dfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
dfs goal succ start = undefined

bfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
bfs goal succ start = undefined

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

bestFirstSearch :: (Eq a, Show a, Ord b) => 
                   (a -> Bool) 
                   -> (a -> [a])
                   -> (a -> b) 
                   -> a -> Maybe a
bestFirstSearch goal succ cost start = undefined

solveMaze' :: Ord a => (Maze -> a) -> Maze -> Maybe Maze
solveMaze' cost mz@(Maze (w,h) _ _) =  
    let entry = (1,1)
        exit = (w, h)
    in bestFirstSearch ((==exit) . head . mazePath) 
                       nextPaths
                       cost
                       (mz { mazePath = [entry]})

bfsSolveMaze :: Maze -> Maybe Maze
bfsSolveMaze = undefined

bfsSolveMaze' :: Maze -> Maybe Maze
bfsSolveMaze' mz@(Maze (w,h) _ _) = undefined

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

aStarSolveMaze :: Maze -> Maybe Maze
aStarSolveMaze mz@(Maze (w,h) _ _) = undefined
