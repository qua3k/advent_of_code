{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Advent.D16 where

import Data.Functor ((<&>))
import Data.Hashable (Hashable, hashUsing, hashWithSalt)
import Data.Maybe (fromMaybe)
import GHC.List (foldl')
import Prelude hiding (Left, Right)
import qualified Data.HashSet as Set
import qualified Data.RRBVector as RRB

type Matrix a = RRB.Vector (RRB.Vector a)
type Point = (Int, Int)

data Object = Empty | Forward | Backward | Pipe | Hyphen 
data Direction = Up | Down | Left | Right
  deriving (Eq, Enum)

data Directions = None | One !Direction | Two !(Direction, Direction)

instance Hashable Direction where
  hashWithSalt = hashUsing fromEnum

getObject :: Point -> Matrix Char -> Maybe Object
getObject (y,x) m =
  RRB.lookup y m >>= RRB.lookup x <&> \o -> case o of
    '.' -> Empty
    '/' -> Forward
    '\\'-> Backward
    '|' -> Pipe
    '-' -> Hyphen

getDirection :: Direction -> Object -> [Direction]
getDirection d Empty = [d]
getDirection Up Forward = [Right]
getDirection Up Backward = [Left]
getDirection Up Pipe = [Up]
getDirection Up Hyphen = [Right, Left]
getDirection Down Forward = [Left]
getDirection Down Backward = [Right]
getDirection Down Pipe = [Down]
getDirection Down Hyphen = [Right, Left]
getDirection Left Forward = [Down]
getDirection Left Backward = [Up]
getDirection Left Pipe = [Down, Up]
getDirection Left Hyphen = [Left]
getDirection Right Forward = [Up]
getDirection Right Backward = [Down]
getDirection Right Pipe = [Down, Up]
getDirection Right Hyphen = [Right] 

getNext :: Point -> Direction -> Point
getNext (y,x) Up = (y-1, x)
getNext (y,x) Down = (y+1, x)
getNext (y,x) Left = (y, x-1)
getNext (y,x) Right = (y, x+1)

-- next coordinate with previous object 
getNextDirection :: Point -> Direction -> Matrix Char -> [Direction]
getNextDirection n d m = fromMaybe [] $ getDirection d <$> getObject n m

filterInsert :: Hashable a => ([a], Set.HashSet a) -> ([a], Set.HashSet a)
filterInsert ([],s) = ([],s)
filterInsert ((x:xs),s) =
  if Set.member x s then
    filterInsert (xs,s)
  else
    let (xs',s') = filterInsert (xs,Set.insert x s) in
    (x:xs',s')

searchBoard :: Point -> Direction -> Matrix Char -> Set.HashSet (Point, Direction)
  -> Set.HashSet (Point, Direction)
searchBoard pt dir m set = case filterInsert (map ((,) next) ds, set) of
    ([], s) -> s
    (ks',s) -> foldl' (\a (p,d) -> 
      searchBoard p d m a) s ks'
  where
    next = getNext pt dir
    ds = getNextDirection next dir m

search :: Point -> Direction -> Matrix Char -> Int
search p d m = Set.size $ Set.map fst
  $ searchBoard p d m Set.empty

toMatrix :: [[a]] -> Matrix a
toMatrix = RRB.map (RRB.fromList) . RRB.fromList

findEnergy :: String -> Int
findEnergy = search (0,-1) Right . toMatrix . lines

findMostEnergy :: String -> Int
findMostEnergy s = max up $ max down $ max left right 
  where
    m = toMatrix $ lines s
    cs = length m - 1
    rs = (length $ RRB.index 0 m) - 1

    left = maximum $ map (\p -> search p Left m) $ zip [0..cs] $ repeat (rs+1)
    right = maximum $ map (\p -> search p Right m) $ zip [0..cs] $ repeat (-1)
    down = maximum $ map (\p -> search p Down m) $ zip (repeat (-1)) [0..rs]
    up = maximum $ map (\p -> search p Up m) $ zip (repeat (cs+1)) [0..rs]
