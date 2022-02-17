{-# LANGUAGE BlockArguments, LambdaCase #-}

module Data.CA.Pattern where

import qualified Data.List as List
import qualified Data.Vector as Vec
import Data.Vector (Vector)

data Cell = Dead | Alive
  deriving (Eq)

isDead :: Cell -> Bool
isDead = \case
  Dead -> True
  Alive -> False

isAlive :: Cell -> Bool
isAlive = \case
  Dead -> False
  Alive -> True

newtype Pattern = Pattern [Vector Cell]

lift :: ([Vector Cell] -> [Vector Cell]) -> Pattern -> Pattern
lift f (Pattern rows) = Pattern (f rows)

maxLength :: [Vector a] -> Int
maxLength = foldr (max . length) 0

padRight :: Int -> a -> Vector a -> Vector a
padRight n x vec = let
  amount = n - length vec
  in vec <> Vec.replicate amount x

fromList :: [[Cell]] -> Pattern
fromList list = let
  rows = map Vec.fromList list
  width = maxLength rows
  padded = map (padRight width Dead) rows
  in Pattern padded

toList :: Pattern -> [[Cell]]
toList (Pattern rows) = map Vec.toList rows

trimTop :: Pattern -> Pattern
trimTop = lift (dropWhile (all isDead))

trimBottom :: Pattern -> Pattern
trimBottom = lift (List.dropWhileEnd (all isDead))

firstCell :: Vector Cell -> Cell
firstCell row
  | Vec.null row = Dead
  | otherwise = Vec.head row

lastCell :: Vector Cell -> Cell
lastCell row
  | Vec.null row = Dead
  | otherwise = Vec.last row

dropFirst :: Vector a -> Vector a
dropFirst = Vec.drop 1

dropLast :: Vector a -> Vector a
dropLast row = Vec.slice 0 (Vec.length row) row

trimLeftList :: [Vector Cell] -> [Vector Cell]
trimLeftList rows
  | all (isDead . firstCell) rows = trimLeftList (map dropFirst rows)
  | otherwise = rows

trimRightList :: [Vector Cell] -> [Vector Cell]
trimRightList rows
  | all (isDead . lastCell) rows = trimRightList (map dropLast rows)
  | otherwise = rows

trimLeft :: Pattern -> Pattern
trimLeft = lift trimLeftList

trimRight :: Pattern -> Pattern
trimRight = lift trimRightList
