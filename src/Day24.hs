module Day24 where

import Utils
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Function
import HeapIndexed
--import Search
import Algorithm.Search
import System.TimeIt
import Debug.Trace


data Celltype = Wall | Path | Label Int deriving (Eq, Show, Ord)

data Cell = Cell {ct :: Celltype, ns :: [(Coord, Int)]} deriving (Eq, Show, Ord)
type Grid = Map Coord Cell
data Node = Lab Coord Int | Junction Coord

isLabel :: Cell -> Bool
isLabel (Cell (Label _) _) = True
isLabel _ = False

showGrid :: Grid -> String
showGrid g = intercalate "\n" $ (\(p,c) -> show p ++ ": " ++ show c) <$> M.toList g


parseGraph :: Grid -> [(Coord, Cell)] -> Grid
parseGraph gin nodesIn
  | null nodesOut = gout
  | otherwise = parseGraph gout nodesOut
  where
    gout = go gin nodesIn
    nodesOut :: [(Coord, Cell)]
    nodesOut = M.toList $ M.filter (\(Cell t ns) -> t == Path && length ns ==2 ) gout
    go :: Grid -> [(Coord, Cell)] -> Grid
    go grid [] = grid
    go grid ((pos, todelete):rem)
      | length (ns todelete) /= 2 = error "Should be 2 neighbours"
      | otherwise = go (M.delete pos updateFm) $ filter (\(c,_) -> c /= to && c /= fm) rem
      where
        (to, d) = head $ ns todelete
        (fm, e) = head $ tail $ ns todelete
        updateTo, updateFm :: Grid
        updateTo = M.insertWith (\_ (Cell t ns) -> 
                                  Cell t $ update (fm, d+e)
                                                  (fromMaybe (error "Error in updateTo") $ findIndex (\(c,_)-> c==pos) ns)
                                                  ns
                                ) to dummyCell grid 
        updateFm = M.insertWith (\_ (Cell t ns) -> 
                                  Cell t $ update (to,d+e)
                                                  (fromMaybe (error "Error in updateFm") $ findIndex (\(c,_) -> c==pos) ns
                                                  ) ns 
                                ) fm dummyCell updateTo 
        dummyCell :: Cell
        dummyCell = Cell Wall []                                


xmax, ymax :: Int
xmax = 179
ymax = 40

parseCelltype :: Char -> Celltype
parseCelltype '.' = Path
parseCelltype '#' = Wall
parseCelltype c = Label $ read [c]


parseGrid :: [String] -> Grid
parseGrid css =  M.fromList $ concatMap (\(y, cs) -> (\(x,c) -> ((x,y), Cell (parseCelltype c) [])) <$> zip [0..] cs) (zip [0..] css)

addNeighbours :: Grid -> Grid
addNeighbours g = M.mapWithKey (\c (Cell t _) -> Cell t ((,1) <$> filter (`elem` gc) (neighbours4 c))) g
  where
    gc = M.keys g


prune :: Grid -> Grid
prune gin
  | null remove = gin
  | otherwise = prune gout
  where
    remove = M.toList $ M.filter (\(Cell t ns) -> (t==Path) && (length ns == 1)) gin
    gout = foldl deleteCoordAt gin remove
    deleteCoordAt :: Grid -> (Coord, Cell) -> Grid
    deleteCoordAt g (pos, Cell t [(cn,_)]) = M.insertWith (\new (Cell t ms) -> Cell t $ filter (\(mc, _) -> mc/=pos) ms) 
                                                          cn dummy $ M.delete pos g
    dummy :: Cell
    dummy = Cell Wall []


removedups :: Cell -> Cell
removedups (Cell t ns) = Cell t newns
  where
    newns = minimumBy (compare `on` snd) <$> groupBy (\x y -> fst x == fst y) ns

type State = (Grid, Coord)

search :: Grid -> Coord -> Coord -> Dist
search g start end = fst ret
  where
    state = (g, start)
    next (g, pos) = (g,) . fst <$> ns (g M.! pos)
    cost (g1, p1) (_, p2) =  go (ns (g M.! p1))
      where
        go ((n, c):xs) = if n==p2 then c else go xs
    remaining (g, pos) = manhattan pos end
    finish (g, pos) = pos == end
    --ret = fromMaybe (error "Error in Dijkstra") $ dijkstra next cost finish state
    ret = fromMaybe (error "Error in Dijkstra") $ aStar next cost remaining finish state

type Dist = Int
type Path = (Int, Int, Maybe Dist)

paths :: [Coord] -> Grid -> Map (Int, Int) Dist
paths cs g = go M.empty [0..maxLabel]
  where
    maxLabel = M.size (M.filter isLabel g) - 1
    go acc [] = acc
    go acc [x] = acc
    go acc (x:y:zs) = go (M.union acc $ M.union (M.fromList add1) (M.fromList add2)) (y:zs)
      where
        add1 = (\w -> ((x, w), search g (cs!!x) (cs!!w))) <$> (y:zs)
        add2 = (\((x,y), d) -> ((y,x), d)) <$> add1


bestPath :: Bool -> [Int] -> Map (Int, Int) Dist -> (Dist, [(Int, [Int])])
bestPath return0 all ps = fromMaybe (error "Error in best path") ret
  where
    initialState :: (Int, [Int])
    initialState = (0, []) -- current label and labels collected
    next (p, ls)
      | return0 && length ls + 1 == length all = [(0, tail all)]
      | otherwise = (, p:ls) <$> all \\ (p:ls)
    cost (l, ls) (m, ms) =  ps M.! (l, m)
    finish (pos, ls) = (length ls + 1 == length all) && (not return0 || pos == 0)
    ret = dijkstra next cost finish initialState


day24 :: IO ()
day24 = do
  ls <- getLines 24
  --let ls = test
  let g = addNeighbours . M.filter (\(Cell c _) -> c /= Wall) $ parseGrid ls
      h = prune g
      nodes2 :: [(Coord, Cell)]
      nodes2 = M.toList $ M.filter (\(Cell t ns) -> t == Path && length ns ==2 ) h
      l = parseGraph h nodes2
      m = removedups <$> l
      labs = fst <$> sortBy (compare `on` ct . snd) (M.toList (M.filter isLabel m))
      maxLabel = length labs - 1
      ps = paths labs m

  timeIt $ putStrLn $ "Day24: part1: " ++ show (fst $ bestPath False [0..maxLabel] ps)
  timeIt $ putStrLn $ "Day24: part2: " ++ show (fst $ bestPath True [0..maxLabel] ps)
  
  return ()


test = ["###########"
      , "#0.1.....2#"
      , "#.#######.#"
      , "#4.......3#"
      , "#.#######.#"
      , "#....##5..#"
      , "###########"]

test2 = [
    "###################################################################################################################################################################################"
  , "#...............#.........#...#...#.......#.......#.#...#.........#...........#...............#.#.#.......#.......#.....#...........#...#...#.......#.......#...#....2#...........#"
  , "#.#.#.#.#.###.#.#####.#.#.#.###.#.#.#######.###.#.#.#.#.#.#.###.#.#.###.###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#####.#.#.#.#.#.###.#.#.#.#####.###.#.#.#.#.#.#.#####.#.#.###.#######"
  , "#....1#...#.#...#.......#.......#...#.......#.....#...#.#...#...#.......#.............#.....#...#.#.........#...#.....#.....#.........#...#...#.....#...#.#...#.#.#...#.........#.#"
  , "#######.#.#.###.#.#######.#####.#.#.#.#.###.#.###.#.#####.#.#.#.#.#.#########.###.#.#.#.###.#.#.#.###.#.###.#.#.#.#.#.#######.#.###########.#.#.###.#.###.#.#.#.#.#.#.#.###.#.#.#.#"
  , "#...#.....#.#...#.......#.#...#.....#.....#.......#.#...#...........#.........#.....#.........#.#.#...#.....#.....#.......#...#.............#...#.#.#.....#...#.#...#.#.....#.....#"
  , "#.###.###.#.#.#.#####.#.#.#.#.#.#.#.#.###.###.#.#.#.#.#.###.#.###.#####.#.#####.###.#####.#.#.#.#.#.#.#####.#.###.#.###.#.###.#.#.###.#.#.#.#.#.#.###.###.#.###.#.###########.#.###"
  , "#...........#.......#...........#.#...#...#.........#.......#.....#...............#.....#...#.........#...#.#...#.....#...#...............#.#.....#.........#.........#.........#3#"
  , "#####.#####.#######.#.#######.#.#.###.#.#.#.#.#########.#####.#####.#######.#.###.#.#.#.#.###.#.#.#.#.#.#.###.#.#.#.#.#.#.###.#####.#######.###.###.#.#####.#.#.###.#.#.#######.#.#"
  , "#...........#.........#.........#...........#...#.....#.....#...................#.......#.........#.#.#.#.#.#...#.#.........#.#...#.....#...#.#.#.#.....#.#.#...#.#.....#.#.......#"
  , "#.###.#####.###.#.#.#.#.#.#.#####.###.#.#####.#.#.#.#####.###.###.###.#.#######.#.#.###.#.#.#######.#.#.#.#.#####.#.#.#.#.#.#.###.#######.###.###.#######.#.#.###.#####.#.###.###.#"
  , "#.....#...............#.#...#.....#...............#.......#.........#.#.#.........#.......#.....#...#.#.....#.....#.#...#.......#.#.#.#...#.....#.......#.#.#.#.#.#.#.#.......#...#"
  , "#.#.###.#.#.#.#.#####.#####.#.#.#.#.#.#####.#.#####.#.#.###.#####.#.#.#.#.###.#####.#.#.#.#.#.#.#.###.#####.#.#.###.#.#.###.#.#.#.#.#.#########.#######.#.#.###.#.#.#.#.#.#.###.#.#"
  , "#.#...#...#...#.............#.#.#.........#...#.........#.#.#...#...#...#.........#.#.....#.....#...........#.....#.#.....#.#...#.......#.#.......#.....#.#.#...#...#...#.#.......#"
  , "#.#.#.#.###.###.###.#####.#.#.#.#.#####.#.###.###.#.#.#.#.###.#.#.#.###.#.#.#####.#.#.#.###.#######.###.###.#.###.###.###.#.#.#.###.###.#.#.###.#.#.#.#.#.#.#####.#.###.#.#.#.#####"
  , "#...........#.......#.....#...#...#.......#.......#.........#...........#.#...#...#.#...#.........#.#.#.....#.......#.#...#...#.........#.....#.#.#.....#.........#.#...#.#...#...#"
  , "#.###.###.#.#.###.#.#.###.#####.#.#.###.#.###.###.#.###.###.#####.#.#.#.#.#.#.#.#.#.###.#.#######.#.#.###.#.#.###.#.#.#.#####.#.#.#####.#.#.#.#.#######.#.#.#.#.###.#.#.###.#.#.#.#"
  , "#.#0......#.#...............#.....#.#.#...#...#...................#...#.#...#...#.....#...#...#.#...#...........#.....#...#.....#.#.....#...#...#.....#...#.....#.......#...#.....#"
  , "#.###.#.#####.#####.#########.###.#.#.#######.#.#.#.#####.###.#.###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.###.#.#.###.#.#.#######.#.#.###.#.#.#.#.#.#.#.#.#.###########.#.###.#.#.#.#.#.###.#"
  , "#.....#.............#.......#.#.....#.....#.#...#...#.#.#.....#.......#...#...#.#.......#...#.....#.....#...#.#.#...#...#.#.#.....#.#...#.....#.#...#.#.....#.#...#.......#...#...#"
  , "#.#.#.###.#.#.###.#.#.#.###.#.#.#####.#####.#.#.#####.#.#.#.#.###.#.#.###.#####.#.###.###.###.###.###.###.#.#.###.###.#.###.#.#.#.#####.#####.#.#.#.#.#.#.#.#.#.#.#.###.###.#.#.#.#"
  , "#.....#.#...#...#...#...#...#.....#.........#.......#...#...#.#.#...#.#.....#.....#.....#.....#...#...#.......#.................#.....#...#.....#...#.........#.#...#...#...#...#.#"
  , "###.#.#.###.#.#.#####.#.###.#.#.#.#.#.###.###.###.#.#.#.#.#.#.#.###.###.###.#.#.#.#.#.#####.#####.#.#####.###.#####.###########.###.#.#.###.#.#.#.#.#######.#.#.#.###.#.#.#.#.#####"
  , "#...#.#.......#.#.......#...#.....#.#.#...........#.#.#...#.........#...#...#.#...#.#.............#.............#...#.#...#...........#.#...#...#...#...#.#.#.....#.........#.#...#"
  , "#.#.#.#.#######.#.#.#.#.###.#.###.#.#.#.#######.###.#.#.#.#.#.###########.###.#.###.#.#.#.###.#########.#.#######.###.#.#.#.#.###.#.#.#.#.#.#.###.#.#.###.#.#######.#.#####.###.#.#"
  , "#...............#...#...#.....#...#.............#.#.#.#...#.#...#.#.......#.....#...#...#.#...#.#.........#.......#.#.....#...#.#.#.#...#.#.#.......#.#.#.....#.......#.#...#...#.#"
  , "###########.#.#.###.#####.#########.#####.#.###.#.#.#.#.#.#.###.#.#.#####.#####.#.#.###.###.###.#.#.#.#.#.#####.###.#####.#.###.#######.###.###.#.#.###.#.#.#.#.#.#.###.#.#.#.###.#"
  , "#.#...#.........#.#.......#...#.....#.#...#...#.......#.........#...#.#.........#.#.#.........#.#.#...#.....#...#.........#.#...#...#...#...#.....#...#.#.#.....#...#...#...#.....#"
  , "#.#.#.#.#.###.#.#.#######.#.#.#.###.#.#.#####.#######.#.#.#.#.#.###.#.###.#.#.#.#########.#.#.#.#.#.#.#.#.#.#.###.#.#.#.###.#.#.#.###.#.#.#.#.#.#.#.#.#.#.#####.###.#.#.#.#.#.###.#"
  , "#...#.#...........#.......#...........#.......#.......#...#...#.#.#.....#.#...........#.#.#.....#.#.........#.....#.....#...#.............#...#...#.....#.....#.....#...#.#.#.#...#"
  , "#.#.#####.#.#####.#.#####.#########.###.#.###.###.#.#.#.#.#.###.#.#.#.#.#.#######.#.#.#.###.###.#.#.#####.#####.###.###.#.#######.#########.#####.#####.###.#.#######.#.#.#.#####.#"
  , "#.#.......#...#...#.....#.#.#.......#...#.#.......#.....#.....#...........#...#...#.#.....#.#.#...#.........#.........#.#.....#...#.........#...#.#.#...#.....#.#.....#.#.........#"
  , "#.#.#.#.#.#.#.#.#######.#.#.###.#.#.#.#.#.#.#.#.#.#.#.#.#######.#.#.#.#.#.#.#.#.#.#.#.#####.#.###.#####.#####.#.#.###.#.###.#.#.#.#.#.#.#.###.#.#.#.###.#.###.#.#.#.###.###.#.#.#.#"
  , "#.#.#.#...#.#.....#.#.#.....#.................#...#.#.#.#...#...........#.#...#.......#...........#...#.........#.#.#.......#...#.......#...#.#...#.#...#...#.#.....#.......#...#.#"
  , "#.###.#.###.#.#.###.#.#.#.###.#########.#.#.#.#.#.#.#.#.#.#.#.#####.#.#.###.#.#.###.###.#.#.#.#.#.#.#####.#.#.#.###.#.#.#.#.#.#.#.#####.###.#.#.###.#####.###.#.#############.#.#.#"
  , "#.....#...#.#.........#.......#.....#...#.....#.#.......#...........#.......#.#.#.......#.....#.#.#.......#.....#...#...#...#.....#...#.....#.......#.........#.......#5#.......#.#"
  , "#.#####.#.#.#.###.###########.#.###.#.#####.#####.###.#####.#################.#.#.#.#.#.###.#####.###.###.###.#.###.###.#.#####.#.#.#.###.#.#####.#.###.#.###.#.###.#.#.#.#.#.###.#"
  , "#.#.....#...#.........#.....#.#.......#...#...#4#...#...#.#.#...#.....#...#.#...#.........#...#.............#...#.#.#.....#.........#.#.....#...........#.....#.#...#...#.#.....#.#"
  , "#.#.#.#.#.#.#########.#.#.#.#.#.#.#####.#.###.#.###.###.#.#.#.#.#.#.#.###.#.#######.#####.#.#.###.#.#.###.#.###.#.#.#.#####.#.###.#.#.###.###.#####.#.#.#.#####.#.#.#.###.#.###.#.#"
  , "#.#...#.....#.......#.....#...#...#.............#.#.#.....#...#...#...#.....#.#.#...#.....#...#...#.#.#.#...#.#.........#.#.......#.......#...#...#.......#.....#.#.#.....#...#...#"
  , "###################################################################################################################################################################################"]


test3 = ["###################################################################################################################################################################################"
  , "#...............#.........#...#...#.......#.......#.#...#.........#...........#...............#.#.#.......#.......#.....#...........#...#...#.......#.......#...#....2#...........#"
  , "#.#.#.#.#.###.#.#####.#.#.#.###.#.#.#######.###.#.#.#.#.#.#.###.#.#.###.###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#####.#.#.#.#.#.###.#.#.#.#####.###.#.#.#.#.#.#.#####.#.#.###.#######"
  , "#....1#...#.#...#.......#.......#...#.......#.....#...#.#...#...#.......#.............#.....#...#.#.........#...#.....#.....#.........#...#...#.....#...#.#...#.#.#...#.........#.#"
  , "#######.#.#.###.#.#######.#####.#.#.#.#.###.#.###.#.#####.#.#.#.#.#.#########.###.#.#.#.###.#.#.#.###.#.###.#.#.#.#.#.#######.#.###########.#.#.###.#.###.#.#.#.#.#.#.#.###.#.#.#.#"
  , "#...#.....#.#...#.......#.#...#.....#.....#.......#.#...#...........#.........#.....#.........#.#.#...#.....#.....#.......#...#.............#...#.#.#.....#...#.#...#.#.....#.....#"
  , "#.###.###.#.#.#.#####.#.#.#.#.#.#.#.#.###.###.#.#.#.#.#.###.#.###.#####.#.#####.###.#####.#.#.#.#.#.#.#####.#.###.#.###.#.###.#.#.###.#.#.#.#.#.#.###.###.#.###.#.###########.#.###"
  , "#...........#.......#...........#.#...#...#.........#.......#.....#...............#.....#...#.........#...#.#...#.....#...#...............#.#.....#.........#.........#.........#3#"
  , "#####.#####.#######.#.#######.#.#.###.#.#.#.#.#########.#####.#####.#######.#.###.#.#.#.#.###.#.#.#.#.#.#.###.#.#.#.#.#.#.###.#####.#######.###.###.#.#####.#.#.###.#.#.#######.#.#"
  , "#...........#.........#.........#...........#...#.....#.....#...................#.......#.........#.#.#.#.#.#...#.#.........#.#...#.....#...#.#.#.#.....#.#.#...#.#.....#.#.......#"
  , "#.###.#####.###.#.#.#.#.#.#.#####.###.#.#####.#.#.#.#####.###.###.###.#.#######.#.#.###.#.#.#######.#.#.#.#.#####.#.#.#.#.#.#.###.#######.###.###.#######.#.#.###.#####.#.###.###.#"
  , "#.....#...............#.#...#.....#...............#.......#.........#.#.#.........#.......#.....#...#.#.....#.....#.#...#.......#.#.#.#...#.....#.......#.#.#.#.#.#.#.#.......#...#"
  , "#.#.###.#.#.#.#.#####.#####.#.#.#.#.#.#####.#.#####.#.#.###.#####.#.#.#.#.###.#####.#.#.#.#.#.#.#.###.#####.#.#.###.#.#.###.#.#.#.#.#.#########.#######.#.#.###.#.#.#.#.#.#.###.#.#"
  , "#.#...#...#...#.............#.#.#.........#...#.........#.#.#...#...#...#.........#.#.....#.....#...........#.....#.#.....#.#...#.......#.#.......#.....#.#.#...#...#...#.#.......#"
  , "#.#.#.#.###.###.###.#####.#.#.#.#.#####.#.###.###.#.#.#.#.###.#.#.#.###.#.#.#####.#.#.#.###.#######.###.###.#.###.###.###.#.#.#.###.###.#.#.###.#.#.#.#.#.#.#####.#.###.#.#.#.#####"
  , "#...........#.......#.....#...#...#.......#.......#.........#...........#.#...#...#.#...#.........#.#.#.....#.......#.#...#...#.........#.....#.#.#.....#.........#.#...#.#...#...#"
  , "#.###.###.#.#.###.#.#.###.#####.#.#.###.#.###.###.#.###.###.#####.#.#.#.#.#.#.#.#.#.###.#.#######.#.#.###.#.#.###.#.#.#.#####.#.#.#####.#.#.#.#.#######.#.#.#.#.###.#.#.###.#.#.#.#"
  , "#.#0......#.#...............#.....#.#.#...#...#...................#...#.#...#...#.....#...#...#.#...#...........#.....#...#.....#.#.....#...#...#.....#...#.....#.......#...#.....#"
  , "#.###.#.#####.#####.#########.###.#.#.#######.#.#.#.#####.###.#.###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.###.#.#.###.#.#.#######.#.#.###.#.#.#.#.#.#.#.#.#.###########.#.###.#.#.#.#.#.###.#"
  , "#.....#.............#.......#.#.....#.....#.#...#...#.#.#.....#.......#...#...#.#.......#...#.....#.....#...#.#.#...#...#.#.#.....#.#...#.....#.#...#.#.....#.#...#.......#...#...#"
  , "#.#.#.###.#.#.###.#.#.#.###.#.#.#####.#####.#.#.#####.#.#.#.#.###.#.#.###.#####.#.###.###.###.###.###.###.#.#.###.###.#.###.#.#.#.#####.#####.#.#.#.#.#.#.#.#.#.#.#.###.###.#.#.#.#"
  , "#.....#.#...#...#...#...#...#.....#.........#.......#...#...#.#.#...#.#.....#.....#.....#.....#...#...#.......#.................#.....#...#.....#...#.........#.#...#...#...#...#.#"
  , "###.#.#.###.#.#.#####.#.###.#.#.#.#.#.###.###.###.#.#.#.#.#.#.#.###.###.###.#.#.#.#.#.#####.#####.#.#####.###.#####.###########.###.#.#.###.#.#.#.#.#######.#.#.#.###.#.#.#.#.#####"
  , "#...#.#.......#.#.......#...#.....#.#.#...........#.#.#...#.........#...#...#.#...#.#.............#.............#...#.#...#...........#.#...#...#...#...#.#.#.....#.........#7#...#"
  , "#.#.#.#.#######.#.#.#.#.###.#.###.#.#.#.#######.###.#.#.#.#.#.###########.###.#.###.#.#.#.###.#########.#.#######.###.#.#.#.#.###.#.#.#.#.#.#.###.#.#.###.#.#######.#.#####.###.#.#"
  , "#...............#...#...#.....#...#.............#.#.#.#...#.#...#.#.......#.....#...#...#.#...#.#.........#.......#.#.....#...#.#.#.#...#.#.#.......#.#.#.....#.......#.#...#...#.#"
  , "###########.#.#.###.#####.#########.#####.#.###.#.#.#.#.#.#.###.#.#.#####.#####.#.#.###.###.###.#.#.#.#.#.#####.###.#####.#.###.#######.###.###.#.#.###.#.#.#.#.#.#.###.#.#.#.###.#"
  , "#.#...#.........#.#.......#...#.....#.#...#...#.......#.........#...#.#.........#.#.#.........#.#.#...#.....#...#.........#.#...#...#...#...#.....#...#.#.#.....#...#...#...#.....#"
  , "#.#.#.#.#.###.#.#.#######.#.#.#.###.#.#.#####.#######.#.#.#.#.#.###.#.###.#.#.#.#########.#.#.#.#.#.#.#.#.#.#.###.#.#.#.###.#.#.#.###.#.#.#.#.#.#.#.#.#.#.#####.###.#.#.#.#.#.###.#"
  , "#...#.#...........#.......#...........#.......#.......#...#...#.#.#.....#.#...........#.#.#.....#.#.........#.....#.....#...#.............#...#...#.....#.....#.....#...#.#.#.#...#"
  , "#.#.#####.#.#####.#.#####.#########.###.#.###.###.#.#.#.#.#.###.#.#.#.#.#.#######.#.#.#.###.###.#.#.#####.#####.###.###.#.#######.#########.#####.#####.###.#.#######.#.#.#.#####.#"
  , "#.#.......#...#...#.....#.#6#.......#...#.#.......#.....#.....#...........#...#...#.#.....#.#.#...#.........#.........#.#.....#...#.........#...#.#.#...#.....#.#.....#.#.........#"
  , "#.#.#.#.#.#.#.#.#######.#.#.###.#.#.#.#.#.#.#.#.#.#.#.#.#######.#.#.#.#.#.#.#.#.#.#.#.#####.#.###.#####.#####.#.#.###.#.###.#.#.#.#.#.#.#.###.#.#.#.###.#.###.#.#.#.###.###.#.#.#.#"
  , "#.#.#.#...#.#.....#.#.#.....#.................#...#.#.#.#...#...........#.#...#.......#...........#...#.........#.#.#.......#...#.......#...#.#...#.#...#...#.#.....#.......#...#.#"
  , "#.###.#.###.#.#.###.#.#.#.###.#########.#.#.#.#.#.#.#.#.#.#.#.#####.#.#.###.#.#.###.###.#.#.#.#.#.#.#####.#.#.#.###.#.#.#.#.#.#.#.#####.###.#.#.###.#####.###.#.#############.#.#.#"
  , "#.....#...#.#.........#.......#.....#...#.....#.#.......#...........#.......#.#.#.......#.....#.#.#.......#.....#...#...#...#.....#...#.....#.......#.........#.......#5#.......#.#"
  , "#.#####.#.#.#.###.###########.#.###.#.#####.#####.###.#####.#################.#.#.#.#.#.###.#####.###.###.###.#.###.###.#.#####.#.#.#.###.#.#####.#.###.#.###.#.###.#.#.#.#.#.###.#"
  , "#.#.....#...#.........#.....#.#.......#...#...#4#...#...#.#.#...#.....#...#.#...#.........#...#.............#...#.#.#.....#.........#.#.....#...........#.....#.#...#...#.#.....#.#"
  , "#.#.#.#.#.#.#########.#.#.#.#.#.#.#####.#.###.#.###.###.#.#.#.#.#.#.#.###.#.#######.#####.#.#.###.#.#.###.#.###.#.#.#.#####.#.###.#.#.###.###.#####.#.#.#.#####.#.#.#.###.#.###.#.#"
  , "#.#...#.....#.......#.....#...#...#.............#.#.#.....#...#...#...#.....#.#.#...#.....#...#...#.#.#.#...#.#.........#.#.......#.......#...#...#.......#.....#.#.#.....#...#...#"
  , "###################################################################################################################################################################################"]
