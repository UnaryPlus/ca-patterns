{-|
The functions in this module allow you to create, transform,
and combine CA patterns.
-}

{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module Data.CA.Pattern
  ( -- * Cells
    Cell
    -- * Patterns
  , Pattern, lookup, generate, height, width, dimensions, valid
    -- * Vector conversions
  , fromRectVector, fromVector, toVector
    -- * List conversions
  , fromRectList, fromList, toList
    -- * Text and string conversions
  , toText, toString
    -- * Trimming
  , trimTop, trimBottom, trimLeft, trimRight, trim
    -- * Cropping and padding
  , setHeight, setWidth, setDimensions
    -- * Transformations
  , reflectX, reflectY, rotateL, rotateR
    -- * Combining
  , combine
  ) where

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
  | Vec.null row = False
  | otherwise = Vec.head row

lastCell :: Vector Cell -> Cell
lastCell row
  | Vec.null row = False
  | otherwise = Vec.last row

{-|
The state of a cell. 'True' represents a live cell and
'False' represents a dead cell.
-}
type Cell = Bool

{-|
A pattern in a 2-dimensional 2-state cellular automaton.
-}
newtype Pattern = Pattern (Vector (Vector Cell))
  deriving (Eq)

instance Show Pattern where
  show pat = "fromList " ++ show (toList pat)

transform :: (Vector (Vector Cell) -> Vector (Vector Cell)) -> Pattern -> Pattern
transform f (Pattern rows) = Pattern (f rows)

{-|
Get the state of one of the cells in a pattern. @lookup 0 0@
returns the cell in the upper-left corner. If the row
or column number is out of range, this function will
return 'Dead'.
-}
lookup
  :: Int -- ^ row
  -> Int -- ^ column
  -> Pattern -> Cell
lookup r c (Pattern rows) = Maybe.fromMaybe False (rows !? r >>= (!? c))

{-|
Generate a pattern from a function.
-}
generate
  :: Int -- ^ height
  -> Int -- ^ width
  -> (Int -> Int -> Cell) -- ^ function taking row and column
  -> Pattern
generate h w f = Pattern (Vec.generate h \r -> Vec.generate w \c -> f r c)

height :: Pattern -> Int
height (Pattern rows) = Vec.length rows

width :: Pattern -> Int
width (Pattern rows)
  | Vec.null rows = 0
  | otherwise = Vec.length (Vec.head rows)

{-|
Get the height and width of a pattern.
-}
dimensions :: Pattern -> (Int, Int)
dimensions pat = (height pat, width pat)

{-|
Test if a pattern is valid, i.e. rectangular.
Some of the functions in this module only behave
properly on rectangular patterns.
-}
valid :: Pattern -> Bool
valid pat@(Pattern rows) = let
  w = width pat
  validRow row = Vec.length row == w
  in all validRow rows

{-|
Convert a vector of rows into a pattern, assuming
the rows are all the same length.
-}
fromRectVector :: Vector (Vector Cell) -> Pattern
fromRectVector = Pattern

{-|
Convert a vector of rows into a pattern. If
the rows are not all the same length, they will
padded with dead cells until the pattern is
rectangular.
-}
fromVector :: Vector (Vector Cell) -> Pattern
fromVector rows = let
  maxWidth = foldr (max . Vec.length) 0 rows
  padded = fmap (padEnd maxWidth False) rows
  in Pattern padded

{-|
Convert a pattern into a vector of rows.
-}
toVector :: Pattern -> Vector (Vector Cell)
toVector (Pattern rows) = rows

{-|
Convert a list of rows into a pattern, assuming
the rows are all the same length.
-}
fromRectList :: [[Cell]] -> Pattern
fromRectList = fromRectVector . Vec.fromList . map Vec.fromList

{-|
Convert a list of rows into a pattern. If
the rows are not all the same length, they will
padded with dead cells until the pattern is
rectangular.
-}
fromList :: [[Cell]] -> Pattern
fromList = fromVector . Vec.fromList . map Vec.fromList

{-|
Convert a pattern into a list of rows.
-}
toList :: Pattern -> [[Cell]]
toList (Pattern rows) = map Vec.toList (Vec.toList rows)

toSomeString :: (Monoid s, IsString s) => (Vector Char -> s) -> Char -> Char -> Pattern -> s
toSomeString makeStr dead alive (Pattern rows) = let
  toChar cell = if cell then alive else dead
  makeLine row = makeStr (fmap toChar row) <> "\n"
  in foldMap makeLine rows

{-|
Convert a pattern into text. For example, @toText \'.' \'Z'@
will replace each dead cell with a @.@ and each live cell
with a @Z@.
-}
toText
  :: Char -- ^ dead cell
  -> Char -- ^ live cell
  -> Pattern -> Text
toText = let
  add c text = Text.singleton c <> text
  in toSomeString (foldr add "")

{-|
Convert a pattern into a string.
-}
toString
  :: Char -- ^ dead cell
  -> Char -- ^ live cell
  -> Pattern -> String
toString = toSomeString Vec.toList

{-|
Remove rows of dead cells from the top of a pattern.
-}
trimTop :: Pattern -> Pattern
trimTop = transform (Vec.dropWhile (all not))

{-|
Remove rows of dead cells from the bottom of a pattern.
-}
trimBottom :: Pattern -> Pattern
trimBottom = transform (dropWhileEnd (all not))

trimLeftV :: Vector (Vector Cell) -> Vector (Vector Cell)
trimLeftV rows
  | not (any firstCell rows) = trimLeftV (fmap dropFirst rows)
  | otherwise = rows

trimRightV :: Vector (Vector Cell) -> Vector (Vector Cell)
trimRightV rows
  | not (any lastCell rows) = trimRightV (fmap dropLast rows)
  | otherwise = rows

{-|
Remove columns of dead cells from the left side of a pattern.
-}
trimLeft :: Pattern -> Pattern
trimLeft = transform trimLeftV

{-|
You get the idea.
-}
trimRight :: Pattern -> Pattern
trimRight = transform trimRightV

{-|
A composition of 'trimTop', 'trimBottom', 'trimLeft', and 'trimRight'.
Removes as many dead cells from the pattern as possible while
keeping it rectangular.
-}
trim :: Pattern -> Pattern
trim = trimTop . trimBottom . trimLeft . trimRight

{-|
Force a pattern to have the given height by removing rows
from the bottom or by adding rows of dead cells.
-}
setHeight :: Int -> Pattern -> Pattern
setHeight h pat =
  case compare h (height pat) of
    LT -> transform (Vec.take h) pat
    EQ -> pat
    GT -> let
      row = Vec.replicate (width pat) False
      in transform (padEnd h row) pat

{-|
Force a pattern to have the given width by remove columns
from the right or by adding columns of dead cells.
-}
setWidth :: Int -> Pattern -> Pattern
setWidth w pat =
  case compare w (width pat) of
    LT -> transform (fmap (Vec.take w)) pat
    EQ -> pat
    GT -> transform (fmap (padEnd w False)) pat

{-|
Set the height and width of a pattern.
-}
setDimensions :: Int -> Int -> Pattern -> Pattern
setDimensions h w = setHeight h . setWidth w

{-|
Reflect vertically, switching the top and the bottom.
-}
reflectY :: Pattern -> Pattern
reflectY = transform Vec.reverse

{-|
Reflect horizontally, switching the left and the right.
-}
reflectX :: Pattern -> Pattern
reflectX = transform (fmap Vec.reverse)

{-|
Rotate counterclockwise by a quarter turn.
-}
rotateL :: Pattern -> Pattern
rotateL pat = let
  (h, w) = dimensions pat
  in generate w h \r c -> lookup c (w - r - 1) pat

{-|
Rotate clockwise by a quarter turn.
-}
rotateR :: Pattern -> Pattern
rotateR pat = let
  (h, w) = dimensions pat
  in generate w h \r c -> lookup (h - c - 1) r pat

{-|
Combine two patterns given a vertical and horizontal offset,
which describe the displacement of the second pattern relative
to the first one.
-}
combine
  :: Int -- ^ vertical offset
  -> Int -- ^ horizontal offset
  -> Pattern -> Pattern -> Pattern
combine y x pat1 pat2 = let
  (y1, y2) = if y < 0 then (-y, 0) else (0, y)
  (x1, x2) = if x < 0 then (-x, 0) else (0, x)
  h = max (height pat1 + y1) (height pat2 + y2)
  w = max (width pat1 + x1) (width pat2 + x2)
  in generate h w \r c -> let
    cell1 = lookup (r - y1) (c - x1) pat1
    cell2 = lookup (r - y2) (c - x2) pat2
    in max cell1 cell2
