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

withDimensions :: Int -> Int -> [Pattern]
withDimensions h w = map Pat.fromRectList (possible2 h w)

combinations :: (Int, Int) -> (Int, Int) -> Pattern -> Pattern -> [Pattern]
combinations (yMin, yMax) (xMin, xMax) pat1 pat2 = let
  combine y x = Pat.combine y x pat1 pat2
  in Ap.liftA2 combine [yMin .. yMax] [xMin .. xMax]
