module Data.CA.List (withDimensions, combinations) where

import qualified Control.Applicative as Ap

import qualified Data.CA.Pattern as Pat
import Data.CA.Pattern (Pattern, Cell(..))

composeN :: Int -> (a -> a) -> a -> a
composeN n f
  | n <= 0 = id
  | otherwise = f . composeN (n - 1) f

possible1 :: Int -> [[Cell]]
possible1 n = composeN n (Ap.liftA2 (:) [Dead, Alive]) [[]]

possible2 :: Int -> Int -> [[[Cell]]]
possible2 h w = let
  rows = possible1 w
  in composeN h (Ap.liftA2 (:) rows) [[]]

{-|
A list of every possible @h@ by @w@ pattern. This function
is necessarily exponential in both arguments, so it's only
practical if the dimensions are very small.
-}
withDimensions
  :: Int -- ^ h
  -> Int -- ^ w
  -> [Pattern]
withDimensions h w = map Pat.fromRectList (possible2 h w)

{-|
Combine two patterns in multiple ways. Useful for creating
a list of glider / still life collisions.
-}
combinations
  :: (Int, Int) -- ^ min and max y offset
  -> (Int, Int) -- ^ min and max x offset
  -> Pattern -> Pattern -> [Pattern]
combinations (yMin, yMax) (xMin, xMax) pat1 pat2 = let
  combine y x = Pat.combine y x pat1 pat2
  in Ap.liftA2 combine [yMin .. yMax] [xMin .. xMax]
