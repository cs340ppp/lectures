{-# LANGUAGE ImplicitParams #-}

module Lect12 where

import Data.Maybe
import Data.List 
import Data.List.Split
import Data.Array
import System.Random
import Control.Monad
import Control.Monad.State
import System.IO
import System.Console.ANSI
import Control.Concurrent
import GHC.IO
import Debug.Trace
import Test.Hspec
import Test.HUnit hiding (Path)
import Test.HUnit.Approx

------------------------------------------------------------------------------

-- generalized, higher-order search
search :: (Eq a, Show a) => 
          (a -> Bool) 
          -> (a -> [a]) 
          -> ([a] -> [a] -> [a])
          -> [a] -> [a] 
          -> Maybe a
search goal adj comb unvisited visited 
  | null unvisited = Nothing
  | goal (head unvisited) = Just $ head unvisited
  | otherwise = let (n:ns) = unvisited
                in search goal adj comb
                          (removeDups (adj n) `comb` ns)
                          (n:visited)
  where removeDups = filter (not . (`elem` (unvisited ++ visited)))


-- best-first search; may use for A* search given an admissible heuristic
bestFirstSearch :: (Eq a, Show a, Ord b) => 
                   (a -> Bool) 
                   -> (a -> [a])
                   -> (a -> b) 
                   -> a -> Maybe a
bestFirstSearch goal succ cost start = 
  search goal succ 
         (\l1 l2 -> sortOn cost $ nub $ l1 ++ l2)
         [start]
         []

------------------------------------------------------------------------------

{- 

  "Sliding Puzzle" problem + solution 

  (see https://en.wikipedia.org/wiki/15_puzzle for a problem description)

-}

type SPPiece = Char
type SPIndex = (Int,Int)

data SPuzzle = SP { 
    dim :: Int,
    pieces :: Array SPIndex SPPiece
  } deriving (Eq)

instance Show SPuzzle where
  show (SP n ps) = ("\n" ++)
                   $ intercalate "\n" 
                   $ map (intersperse ' ') 
                   $ chunksOf n 
                   $ Data.Array.elems ps

emptyPuzzle :: Int -> SPuzzle
emptyPuzzle n = SP n $ listArray ((1,1),(n,n)) 
                     $ (take (n^2-1) ['A'..]) ++ " "


-- NB: simply generating a random puzzle doesn't mean it's solvable
shufflePuzzle :: SPuzzle -> StdGen -> SPuzzle
shufflePuzzle puz@(SP n ps) gen = foldl rand puz 
                                        (take (n^3) $ randoms gen :: [Int])
  where rand puz r = let alts = spuzMoves puz
                     in alts !! (r `mod` (length alts))


spuzLoc :: SPuzzle -> SPPiece -> (Int,Int)
spuzLoc (SP n ps) p = let (Just i) = findIndex (==p) $ Data.Array.elems ps
                      in ((i `div` n)+1, (i `mod` n)+1)


spuzMoves :: SPuzzle -> [SPuzzle]
spuzMoves puz@(SP n ps) = 
  let (r,c) = spuzLoc puz ' '
      adj = [(r+dr,c+dc) | dr <- [-1,0,1], dc <- [-1,0,1], 
                           (dr==0 && dc/=0) || (dr/=0 && dc==0),
                           inN (r+dr) && inN (c+dc)]
      inN i = i >= 1 && i <= n
  in [SP n (ps // [((r,c), ps!(r',c')), ((r',c'), ' ')]) | (r',c') <- adj]


-- cost fn = sum of Manhattan distances of pieces from their goal positions
spuzCost :: SPuzzle -> Int
spuzCost puz@(SP n ps) = sum 
                          $ zipWith dist (map (spuzLoc puz) 
                                              (take (n^2-1) ['A'..]))
                          $ [(r,c) | r <- [1..n], c <- [1..n]]
  where dist (r1,c1) (r2,c2) = abs (r1-r2) + abs (c1-c2)


data SPSol = SPSol { solPath :: [SPuzzle] }

instance Show SPSol where
  show (SPSol ps) = show $ head ps

instance Eq SPSol where
  (SPSol (p1:_)) == (SPSol (p2:_)) = p1 == p2


-- sliding puzzle solver using cost fn above as A* heuristic
solvePuzzle :: SPuzzle -> SPSol
solvePuzzle puz@(SP n ps) = 
  let puzDone = emptyPuzzle n
  in fromJust $ bestFirstSearch ((==puzDone) . head . solPath)
                                nextSol
                                cost
                                (SPSol [puz])
  where nextSol (SPSol ps@(p:_)) = [SPSol (np:ps) | np <- spuzMoves p, 
                                                    not (np `elem` ps)]
        cost (SPSol ps@(p:_)) = length ps + spuzCost p


replaySol :: SPSol -> IO ()
replaySol (SPSol ps) = 
  forM_ (reverse ps) $ \p -> do 
    clearScreen
    setCursorPosition 0 0
    print p
    threadDelay $ 10^6

-- try: replaySol $ solvePuzzle $ shufflePuzzle (emptyPuzzle 4) (mkStdGen 25)

------------------------------------------------------------------------------

{- 
  Path-finding problem

  Given a list of locations and distances between them, find the shortest path
  between a start and end location, where the maximum single "hop" distance is given. If no path exists (e.g., if the max hop distance is too small to reach the end location), the search should fail.
-}

{- Locations are named, and have (x,y) coordinates -}
data Location = Loc { locName :: String
                    , locPos ::  (Double,Double) 
                    } deriving (Show, Eq)

{- 
 Test locations:

     | 0 1 2 3 4 5 6 7 8 9
    -|--------------------
    0| A         B         
    1|     C   D
    2|               E
    3|   F                
    4|             G
    5|   H     I
    6|                 J
    7|         K
    8|   L
    9|           M       N
-}
testLocs = [ Loc "A" (0,0)
           , Loc "B" (5,0)
           , Loc "C" (1,2)
           , Loc "D" (1,4)
           , Loc "E" (2,7)
           , Loc "F" (3,1)
           , Loc "G" (4,6)
           , Loc "H" (5,1)
           , Loc "I" (5,4)
           , Loc "J" (6,8)
           , Loc "K" (7,4)
           , Loc "L" (8,1)
           , Loc "M" (9,5)
           , Loc "N" (9,9) ]


{- Distance between two locations -}
distance :: Location -> Location -> Double
distance (Loc _ (x1,y1)) (Loc _ (x2,y2)) = sqrt $ (x1-x2)^2 + (y1-y2)^2


{- Given a location, finds all locations within a given distance -}
within :: Double -> Location -> [Location] -> [Location]
within d loc@(Loc name _) locs = [l | l <- locs, 
                                      locName l /= name, 
                                      distance loc l <= d]


{- A path is a list of locations and the total distance traveled -}
data Path = Path { pathDist :: Double
                 , pathLocs :: [Location]
                 } deriving (Show, Eq)



{- 
  Given a hop distance, a list of locations, and a path, returns a list of 
  paths that extend the given path by one location, where the next location 
  is within the hop distance of the last location in the path. A location 
  can only be visited once in a path.

  The distance of each path is the sum of the distances between each adjacent 
  pair of locations in the path.

  This function is suitable for use as the successor function in a search.

    e.g., extendPath 4 testLocs (Path 0 [head testLocs])
          = [Path {pathDist = 2.23606797749979, 
                   pathLocs = [Loc {locName = "C", locPos = (1.0,2.0)},
                               Loc {locName = "A", locPos = (0.0,0.0)}]},
             Path {pathDist = 3.1622776601683795, 
                   pathLocs = [Loc {locName = "F", locPos = (3.0,1.0)},
                               Loc {locName = "A", locPos = (0.0,0.0)}]}]
-}                
extendPath :: Double -> [Location] -> Path -> [Path]
extendPath hop locs path = undefined


{-
  Given a hop distance, a list of locations, a start location, and an end 
  location, finds the shortest path between the start and end locations. If no path exists, returns Nothing.

  This should be implemented using bestFirstSearch.
-}
findPath :: Double -> [Location] -> Location -> Location -> Maybe Path
findPath hop locs start end = undefined


{-
  Some tests. Run them with `hspec pathFindingSpec`.
-}
pathFindingSpec :: Spec
pathFindingSpec = describe "Path finding solution" $ do
  let start = head testLocs; end = last testLocs
  let ?epsilon = 0.01
  describe "distance" $ do
    it "computes distances" $ do
      distance (Loc "" (0,0)) (Loc "" (5,0)) @?= 5
      distance (Loc "" (0,0)) (Loc "C" (1,2)) @?~ sqrt 5
      distance start end @?~ 12.72

  describe "within" $ do
    it "locates reachable locs" $ do
      within 3 start testLocs `shouldBe` [Loc "C" (1,2)]
      within 4 start testLocs `shouldMatchList` [Loc "C" (1,2), 
                                                 Loc "F" (3,1)]
      within 5 start testLocs `shouldMatchList` [Loc "B" (5,0), 
                                                 Loc "C" (1,2),
                                                 Loc "D" (1,4),
                                                 Loc "F" (3,1)]

  describe "extendPath" $ do
    it "extends a single-loc path by one loc" $ do
      let startPath = Path 0 [start]
      let ps = extendPath 3 testLocs startPath
      length ps @?= 1
      pathDist (head ps) @?~ 2.23
      map locName (pathLocs (head ps)) `shouldBe` ["C", "A"]

    it "extends a single-loc path by two locs" $ do
      let startPath = Path 0 [start]
      let ps = extendPath 4 testLocs startPath
      length ps @?= 2
      map pathDist ps `shouldMatchListApprox` [2.23, 3.16]
      map (map locName . pathLocs) ps `shouldMatchList` [["C", "A"], 
                                                         ["F", "A"]]

    it "extends a two-loc (non-zero length) path" $ do
      let startPath = Path 3 [Loc "C" (1,2), start]
      let ps = extendPath 4 testLocs startPath
      length ps @?= 2
      map pathDist ps `shouldMatchListApprox` [5, 5.23]
      map (map locName . pathLocs) ps `shouldMatchList` [["D", "C", "A"], 
                                                         ["F", "C", "A"]]

    it "extends a three-loc path to all remaining locs (giant hop)" $ do
      let startPath = Path 5 [Loc "D" (1,4), Loc "C" (1,2), start]
      let ps = extendPath 50 testLocs startPath
      length ps @?= 11
      let names = map (map locName . pathLocs) ps
      concat (map head names) `shouldMatchList` "BEFGHIJKLMN"

  describe "findPath" $ do
    it "finds no solution with hop length <= 3" $ do
      findPath 1 testLocs (head testLocs) (last testLocs) @?= Nothing
      findPath 2 testLocs (head testLocs) (last testLocs) @?= Nothing
      findPath 3 testLocs (head testLocs) (last testLocs) @?= Nothing

    it "finds the correct solution with hop length = 4" $ do 
      let p = findPath 4 testLocs start end
      p `shouldSatisfy` isJust
      let Just p' = p
      pathDist p' @?~ 13.83
      map locName (pathLocs p') `shouldBe` ["N","J","G","D","C","A"]

    it "finds the correct solution with hop length = 5" $ do 
      let p = findPath 5 testLocs start end
      p `shouldSatisfy` isJust
      let Just p' = p
      pathDist p' @?~ 13.22
      map locName (pathLocs p') `shouldBe` ["N","J","G","C","A"]

    it "finds the correct solution with hop length = 6" $ do 
      let p = findPath 6 testLocs start end
      p `shouldSatisfy` isJust
      let Just p' = p
      pathDist p' @?~ 13.06
      map locName (pathLocs p') `shouldBe` ["N","G","C","A"]
      
    it "finds the correct solution with hop length = 7" $ do 
      let p = findPath 7 testLocs start end
      p `shouldSatisfy` isJust
      let Just p' = p
      pathDist p' @?~ 12.80
      map locName (pathLocs p') `shouldBe` ["N","I","A"]


shouldMatchListApprox :: (Show a, Ord a, Fractional a, ?epsilon :: a) => 
                         [a] -> [a] -> Assertion
shouldMatchListApprox xs ys = 
  let xs' = sort xs
      ys' = sort ys
  in and (zipWith (\x y -> abs (x - y) < ?epsilon) xs' ys') @? 
     ("expected: " ++ show xs ++ "\n but got: " ++ show ys)
                               
