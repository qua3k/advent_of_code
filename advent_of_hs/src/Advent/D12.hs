module Advent.D12 where

import qualified Data.HashMap.Strict as HM

countPossible :: String -> [Int] -> Int
countPossible str gs = count str (HM.singleton (False,0,gs) 1)
  where
    -- Lazy DFA construction.
    count (x:xs) = count xs 
      . HM.foldlWithKey' (\a k v -> countMachine x k v a) HM.empty
    count [] = HM.foldl' (+) 0  . HM.filterWithKey (\(_,_,g) -> const $ null g)

    countMachine '#' (False,n,g@(hd:tl)) v =
      HM.insertWith (+) (if n + 1 == hd then (True,0,tl) else (False,n+1,g)) v
    countMachine '.' (_,0,g) v = HM.insertWith (+) (False,0,g) v
    countMachine '?' s@(True,_,_) v = countMachine '.' s v
    countMachine '?' s@(False,_,_) v = countMachine '.' s v . countMachine '#' s v
    countMachine _ _ _ = id


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn n xs = hd:splitOn n tl
  where
    (hd,tl) = case break (==n) xs of
              (h,_:t) -> (h,t)
              (h, []) -> (h,[])

parseGroups :: String -> [(String, [Int])]
parseGroups =
  map (\(p, gs) -> (p, map read $ splitOn ',' $ drop 1 gs ))
  . map (break (==' '))
  . lines

sumPossible :: String -> Int
sumPossible =
  sum
  . map (uncurry countPossible)
  . parseGroups

cycleN :: [a] -> Int -> [a] 
cycleN _  0 = []
cycleN xs 1 = xs
cycleN xs n = xs ++ (cycleN xs $ n-1)

sumPossible2 :: String -> Int
sumPossible2 =
  sum
  . map (uncurry countPossible)
  . map (\(p, gs) -> (p ++ cycleN ('?':p) 4, cycleN gs 5))
  . parseGroups
