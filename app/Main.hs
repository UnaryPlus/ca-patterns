module Main where

import Data.CA.Pattern

main :: IO ()
main = putStrLn "Hello, Haskell!"

{-
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module C1_4x4 (main) where

import qualified Data.List as List

import qualified Data.Text as Text
import Data.Text (Text)

data Cell = Dead | Alive
type Pattern = [[Cell]]

isDead :: Cell -> Bool
isDead = \case
  Dead -> True
  _ -> False

firstCell :: [Cell] -> Cell
firstCell = \case
  [] -> Dead
  cell : _ -> cell

trimTop :: Pattern -> Pattern
trimTop = dropWhile (all isDead)

trimBottom :: Pattern -> Pattern
trimBottom = List.dropWhileEnd (all isDead)

trimLeft :: Pattern -> Pattern
trimLeft pattern
  | all (isDead . firstCell) pattern = trimLeft (map (drop 1) pattern)
  | otherwise = pattern

trimRight :: Pattern -> Pattern
trimRight = map (List.dropWhileEnd isDead)

trim :: Pattern -> Pattern
trim = trimLeft . trimRight . trimTop . trimBottom

width :: Pattern -> Int
width = foldr (max . length) 0

height :: Pattern -> Int
height = length

data Symbol = SDead | SAlive | SNewline
  deriving (Eq)

cellToSymbol :: Cell -> Symbol
cellToSymbol = \case
  Dead -> SDead
  Alive -> SAlive

getSymbols :: Pattern -> [Symbol]
getSymbols = List.intercalate [SNewline] . map (map cellToSymbol)

getRuns :: (Eq a) => [a] -> [(Int, a)]
getRuns = let
  add s = \case
    (n, s') : runs | s == s' -> (n + 1, s) : runs
    runs -> (1, s) : runs
  in foldr add []

symbolRuns :: Pattern -> [(Int, Symbol)]
symbolRuns = getRuns . getSymbols

class RLEText a where
  toText :: a -> Text

instance RLEText Int where
  toText = Text.pack . show

instance RLEText Symbol where
  toText = \case
    SDead -> "b"
    SAlive -> "o"
    SNewline -> "$"

instance (RLEText a, RLEText b) => RLEText (a, b) where
  toText (x, y) = toText x <> toText y

instance RLEText a => RLEText [a] where
  toText = foldMap toText

toRLE :: Pattern -> Text -> Text
toRLE pattern rule =
    "x = " <> toText (width pattern) <>
    ", y = " <> toText (height pattern) <>
    ", rule = " <> rule <> "\n" <>
    toText (symbolRuns pattern) <> "!\n"

symmetrica :: Text
symmetrica = "B2-ak3-jnqr4ce/S23"

every4x4 :: [Pattern]
every4x4 = do


main :: IO ()
main = return ()
-}
