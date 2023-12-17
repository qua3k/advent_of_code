{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import System.Environment (getArgs)
import System.IO

import Advent.D12
import Advent.D14
import Advent.D16

solveChallenge :: [Int] -> IO String
solveChallenge (12:1:_) = do
  text <- readFile' "data/12.txt"
  return $ show $ sumPossible text
solveChallenge (12:2:_) = do
  text <- readFile' "data/12.txt"
  return $ show $ sumPossible2 text
solveChallenge (14:1:_) = do
  text <- readFile' "data/14.txt"
  return $ show $ hashSum text 
solveChallenge (14:2:_) = do
  text <- readFile' "data/14.txt"
  return $ show $ calculateFocus text
solveChallenge (16:_) = do
  text <- readFile' "data/16.txt"
  return $ show $ findEnergy text 


main :: IO ()
main = do
  args <- getArgs
  res <- solveChallenge $ map read args
  putStrLn res
