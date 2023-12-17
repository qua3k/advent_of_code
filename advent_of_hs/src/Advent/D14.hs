{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Advent.D14 where

import GHC.List (foldl')
import Advent.D12 (splitOn)

import qualified Data.RRBVector as RRB

hash :: String -> Int
hash = foldl' (\a c -> rem (17 * (a + fromEnum c)) 256) 0

hashSum :: String -> Int
hashSum =  sum . map hash . splitOn ','

type Boxes a = RRB.Vector (RRB.Vector (String, a))

vectorFind :: (Eq a) => a -> RRB.Vector (a, b) -> Maybe Int
vectorFind l v = fst <$> RRB.ifind (\_ -> (==l) . fst) v

addLens :: String -> a -> Boxes a -> Boxes a
addLens l f = RRB.adjust (hash l) $ \b ->
  case vectorFind l b of
    Just i -> RRB.update i t b
    Nothing -> b RRB.|> t
  where
    t = (l,f)

deleteLens :: String -> Boxes Int -> Boxes Int
deleteLens l = RRB.adjust (hash l) $ deleteExists
  where
    deleteExists v = case vectorFind l v of
      Just i -> RRB.deleteAt i v
      Nothing -> v

focusPower :: Boxes Int -> Int
focusPower =
  RRB.ifoldl' (\vi a -> (+) a
    . sum 
    . RRB.imap (\i (_,l) -> (vi+1)*(i+1)*l)) 0

calculateFocus :: String -> Int
calculateFocus =
  focusPower 
  . foldl' (\a b -> case break isOperation b of 
    (l,"-") -> deleteLens l a
    (l,'=':fl) -> addLens l (read fl) a) boxes
  . splitOn ','
  where
    boxes = RRB.replicate 256 RRB.empty
    isOperation '-' = True
    isOperation '=' = True
    isOperation  _  = False
