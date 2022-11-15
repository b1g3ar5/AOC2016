module Utils (
  getLines
  , getRaw
  , getWords
  , getWordsBy
  , getParagraphs
  , splitOn
  , toInt
  , fromInt
  , pad
  , Coord
  , allCoords
  , neighbourCoords4
  , neighbourCoords8
  , splitAt
  , intersections
  , clockTurn
  , antiTurn
  , directions
  , leftOf
  , rightOf
  , above
  , below
  , neighbours4
  , neighbours8
  , span
  , race
  , fixpoint
  , wordsBy
  , Dir(..)
  , toCoord
  , toCoord'
  , Turn(..)
  , manhattan
  , toTurn
  , update
)
where


import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Group
import Data.Hashable
import Control.DeepSeq

--- Things to add

-- Rectangular grid with focus and distributive, representable instances
-- Directions, rotations...

updateWith :: (a -> a) -> Int -> [a] -> [a]
updateWith f pos list = take pos list ++ f (list!!pos) : drop (pos+1) list

update :: a -> Int -> [a] -> [a]
update x pos list = take pos list ++ x : drop (pos+1) list


------------ GET THE INPUT FROM FILE ------------------

getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- readFile $ "./data/" ++ show n ++ ".in"
  return $ f s


getRaw :: Int -> IO String
getRaw = getF id


getWords :: Int -> IO [String]
getWords = getF words

getWordsBy :: (Char -> Bool) -> Int -> IO [String]
getWordsBy f = getF (wordsBy f)


getLines :: Int -> IO [String]
getLines = getF lines


getParagraphs :: Int -> IO [[String]]
getParagraphs = getF (filter (/=[]) . splitOn [""] . lines)


------------------ VARIOUS UTILITY FUNCTIONS --------------------


intersections :: Ord a => [S.Set a] -> S.Set a
intersections ss = foldl' S.intersection (head ss) (tail ss)


fix :: Eq a => (a -> a) -> a
fix f = x where x = f x


-- Should this just call fix somehow?
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x 
  | x ==fx = fx
  | otherwise = fixpoint f fx
  where
    fx = f x


-- This takes 2 predicates isFinished and isOK
-- and a start value and a next function
-- it returns True if isOK happens before isFinished
race :: (a -> Bool) -> (a -> Bool) -> a -> (a -> a) -> Bool
race isFinished isOk x next 
  | isFinished nxt = False
  | isOk nxt = True
  | otherwise = race isFinished isOk nxt next
  where
    nxt = next x



------------------- BOOLEAN / BINARY TO/FROM INT

-- This does conversion units at the front of the list
toInt :: [Bool] -> Integer
toInt [] = 0
toInt bs = 2 * toInt (tail bs) + if head bs then 1 else 0


fromInt :: Integer -> [Bool]
fromInt 0 = [False]
fromInt i = helper i
  where
    helper 0 = []
    helper i = let (q,r) = i `divMod` 2 in (r==1) : helper q


pad :: Integer -> a -> [a] -> [a]
pad n b bs = replicate (fromIntegral n - length bs) b ++ take (fromIntegral n) bs



------------------------ COORDINATE / VECTOR STUFF ------------

-- Traditionally measured down from top left
-- Coords are (x, y)

type Coord = (Int, Int)


manhattan :: Coord -> Coord -> Int
manhattan (x, y) (a, b) = abs (a-x) + abs (b-y)

data Dir = N | E | S | W deriving (Show, Eq, Read, Enum, Bounded)

type Turn = Int -- turn in degrees

toTurn :: Dir -> Turn
toTurn N = 0
toTurn E = 90
toTurn S = 180
toTurn W = 270


fromTurn :: Turn -> Dir
fromTurn 0 = N
fromTurn 90 = E
fromTurn 180 = S
fromTurn 270 = W
fromTurn t = error $ "Not implemented fromTurn for t= " ++ show t

toCoord' :: Dir -> Coord
toCoord' N = (0, -1)
toCoord' S = (0, 1)
toCoord' E = (1, 0)
toCoord' W = (-1, 0)

-- The unit vector pointing in the direction
-- Remember coords start at top left ie. screen coords
toCoord :: Turn -> Coord
toCoord 0 = (0, -1)
toCoord 90 = (1, 0)
toCoord 180 = (0, 1)
toCoord 270 = (-1, 0)
toCoord t = error $ "Not yet implemented toCoord for t= " ++ show t


fromCoord :: Coord -> Turn
fromCoord (0, 1) = 0
fromCoord (1, 0) = 90
fromCoord (-1, 0) = 270
fromCoord (0, -1) = 180
fromCoord c = error $ "Not yet implemented fromCoord for c = " ++ show c


instance Semigroup Turn where
  t1 <> t2 = (t1+t2) `mod` 360

instance Monoid Turn where
  mempty  = 0

instance Group Turn where
  invert t = (360-t) `mod` 360

instance Abelian Turn


instance Num Coord where
  (x1, y1) + (x2, y2) = (x1+x2, y1+y2)
  (x1, y1) - (x2, y2) = (x1-x2, y1-y2)
  (x1, y1) * (x2, y2) = (x1*x2, y1*y2)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger i = (fromInteger i, 0)


euclidian :: Coord -> Double
euclidian (x, y) = sqrt $ fromIntegral (x*x + y*y)


clockTurn :: Coord -> Coord
clockTurn (x, y) = (-y, x)
antiTurn :: Coord -> Coord
antiTurn (x, y) = (y, -x)


neighbourCoords8 :: [Coord]
neighbourCoords8 = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]
neighbourCoords4 :: [Coord]
neighbourCoords4 = [(1,0),(-1,0),(0,1),(0,-1)]


neighbours8 :: Coord -> [Coord]
neighbours8 c = neighbourCoords8 `at` c
neighbours4 :: Coord -> [Coord]
neighbours4 c = neighbourCoords4 `at` c



at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (+ origin) coords


mul :: Int -> Coord -> Coord
mul c (x,y) = (c*x, c*y)


-- All coords in a grid in (x, y) (col, row) order
allCoords :: Int -> Int -> [Coord]
allCoords rows cols = concat $ (\c -> (c,) <$> [0..(rows-1)]) <$> [0..(cols-1)]


directions :: [Coord]
directions = [(0, -1), (0, 1), (1, 0), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]


-- Coordinate start at top left, so y goes up as you go down
leftOf, rightOf, above, below :: Coord -> Coord
leftOf x = x + (-1,0)
rightOf x = x + (1,0)
above x = x + (0,-1)
below x = x + (0,1)


