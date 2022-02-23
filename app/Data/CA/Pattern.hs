{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module Data.CA.Pattern
  ( Cell(..), isDead, isAlive
  , Pattern
  , fromRectVector, fromVector, toVector
  , fromRectList, fromList, toList
  , toText, toString
  , lookup, generate
  , height, width, dimensions, valid
  , trimTop, trimBottom, trimLeft, trimRight
  , setHeight, setWidth, setDimensions
  , reflectX, reflectY, rotateL, rotateR
  , combine )
  where

import Prelude hiding (lookup)
import qualified Data.Maybe as Maybe
import Data.String (IsString)

import qualified Data.Vector as Vec
import Data.Vector (Vector, (!?))

import qualified Data.Text as Text
import Data.Text (Text)

padEnd :: Int -> a -> Vector a -> Vector a
padEnd n val row = row <> Vec.replicate (n - length row) val

dropWhileEnd :: (a -> Bool) -> Vector a -> Vector a
dropWhileEnd f row
  | Vec.null row = Vec.empty
  | f (Vec.last row) = dropWhileEnd f (Vec.init row)
  | otherwise = row

dropFirst :: Vector a -> Vector a
dropFirst = Vec.drop 1

dropLast :: Vector a -> Vector a
dropLast row
  | Vec.null row = Vec.empty
  | otherwise = Vec.init row

firstCell :: Vector Cell -> Cell
firstCell row
  | Vec.null row = Dead
  | otherwise = Vec.head row

lastCell :: Vector Cell -> Cell
lastCell row
  | Vec.null row = Dead
  | otherwise = Vec.last row

data Cell = Dead | Alive
  deriving (Eq, Ord)

isDead :: Cell -> Bool
isDead = \case { Dead -> True; Alive -> False }

isAlive :: Cell -> Bool
isAlive = \case { Dead -> False; Alive -> True }

newtype Pattern = Pattern (Vector (Vector Cell))

transform :: (Vector (Vector Cell) -> Vector (Vector Cell)) -> Pattern -> Pattern
transform f (Pattern rows) = Pattern (f rows)

fromRectVector :: Vector (Vector Cell) -> Pattern
fromRectVector = Pattern

fromVector :: Vector (Vector Cell) -> Pattern
fromVector rows = let
  maxWidth = foldr (max . Vec.length) 0 rows
  padded = fmap (padEnd maxWidth Dead) rows
  in Pattern padded

toVector :: Pattern -> Vector (Vector Cell)
toVector (Pattern rows) = rows

fromRectList :: [[Cell]] -> Pattern
fromRectList = fromRectVector . Vec.fromList . map Vec.fromList

fromList :: [[Cell]] -> Pattern
fromList = fromVector . Vec.fromList . map Vec.fromList

toList :: Pattern -> [[Cell]]
toList (Pattern rows) = map Vec.toList (Vec.toList rows)

toSomeString :: (Monoid s, IsString s) => (Vector Char -> s) -> Char -> Char -> Pattern -> s
toSomeString makeStr dead alive (Pattern rows) = let
  toChar = \case { Dead -> dead; Alive -> alive }
  makeLine row = makeStr (fmap toChar row) <> "\n"
  in foldMap makeLine rows

toText :: Char -> Char -> Pattern -> Text
toText = let
  add c text = Text.singleton c <> text
  in toSomeString (foldr add "")

toString :: Char -> Char -> Pattern -> String
toString = toSomeString Vec.toList

lookup :: Int -> Int -> Pattern -> Cell
lookup r c (Pattern rows) = Maybe.fromMaybe Dead (rows !? r >>= (!? c))

generate :: Int -> Int -> (Int -> Int -> Cell) -> Pattern
generate h w f = Pattern (Vec.generate h \r -> Vec.generate w \c -> f r c)

height :: Pattern -> Int
height (Pattern rows) = Vec.length rows

width :: Pattern -> Int
width (Pattern rows)
  | Vec.null rows = 0
  | otherwise = Vec.length (Vec.head rows)

dimensions :: Pattern -> (Int, Int)
dimensions pat = (height pat, width pat)

valid :: Pattern -> Bool
valid pat@(Pattern rows) = let
  w = width pat
  validRow row = Vec.length row == w
  in all validRow rows

trimTop :: Pattern -> Pattern
trimTop = transform (Vec.dropWhile (all isDead))

trimBottom :: Pattern -> Pattern
trimBottom = transform (dropWhileEnd (all isDead))

trimLeftV :: Vector (Vector Cell) -> Vector (Vector Cell)
trimLeftV rows
  | all (isDead . firstCell) rows = trimLeftV (fmap dropFirst rows)
  | otherwise = rows

trimRightV :: Vector (Vector Cell) -> Vector (Vector Cell)
trimRightV rows
  | all (isDead . lastCell) rows = trimRightV (fmap dropLast rows)
  | otherwise = rows

trimLeft :: Pattern -> Pattern
trimLeft = transform trimLeftV

trimRight :: Pattern -> Pattern
trimRight = transform trimRightV

setHeight :: Int -> Pattern -> Pattern
setHeight h pat =
  case compare h (height pat) of
    LT -> transform (Vec.take h) pat
    EQ -> pat
    GT -> let
      row = Vec.replicate (width pat) Dead
      in transform (padEnd h row) pat

setWidth :: Int -> Pattern -> Pattern
setWidth w pat =
  case compare w (width pat) of
    LT -> transform (fmap (Vec.take w)) pat
    EQ -> pat
    GT -> transform (fmap (padEnd w Dead)) pat

setDimensions :: Int -> Int -> Pattern -> Pattern
setDimensions h w = setHeight h . setWidth w

reflectX :: Pattern -> Pattern
reflectX = transform (fmap Vec.reverse)

reflectY :: Pattern -> Pattern
reflectY = transform Vec.reverse

rotateL :: Pattern -> Pattern
rotateL pat = let
  (h, w) = dimensions pat
  in generate w h \r c -> lookup (h - c) r pat

rotateR :: Pattern -> Pattern
rotateR pat = let
  (h, w) = dimensions pat
  in generate w h \r c -> lookup c (w - r) pat

combine :: Int -> Int -> Pattern -> Pattern -> Pattern
combine y x pat1 pat2 = let
  (y1, y2) = if y < 0 then (-y, 0) else (0, y)
  (x1, x2) = if x < 0 then (-x, 0) else (0, x)
  h = max (height pat1 + y1) (height pat2 + y2)
  w = max (width pat1 + x1) (width pat2 + x2)
  in generate h w \r c -> let
    cell1 = lookup (r - y1) (c - x1) pat1
    cell2 = lookup (r - y2) (c - x2) pat2
    in max cell1 cell2
