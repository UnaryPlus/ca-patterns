{-|
The RLE (run length encoded) format is a common way of representing
patterns in life-like cellular automata. RLE files consist of three
sections:

  1. Zero or more comment lines beginning with @#@

  2. A header of the form @x = [width], y = [height], rule = [rule]@.
  @[width]@ and @[height]@ are natural numbers, and @[rule]@ is the
  rule the pattern is meant to be run in, such as @B36/S23@. The "rule" field
  is optional.

  3. The content of the pattern. @b@ represents a dead cell, @o@ represents
  a live cell, and @$@ denotes the end of a row. A run of identical
  characters can be abbreviated with a number, e.g. @4o@ is short for
  @oooo@ (hence the name "run length encoded"). This section must be
  terminated by a @!@ character.

A glider in the Game of Life could be represented like so:

> #N glider
> x = 3, y = 3, rule = B3/S23
> 3o$2bo$bo!

See this [LifeWiki article](https://conwaylife.com/wiki/Run_Length_Encoded)
for more information.
-}

{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, FlexibleContexts #-}

module Text.RLE (Rule, parse, make, printAll) where

import qualified Data.Char as Char
import qualified Data.String as String
import qualified Control.Applicative as Ap
import Data.Functor.Identity (Identity)
import Data.String (IsString)

import Text.Parsec
  ( Parsec, Stream, runParser
  , (<|>), try, many, many1, manyTill, satisfy
  , anyChar, digit, string, spaces
  )

import qualified Data.Text.IO as IO

import qualified Data.CA.Pattern as Pat
import Data.CA.Pattern (Pattern)

{-|
A string representing a cellular automaton, such as @"B36/S23"@ or
@"B3/S2-i34q"@.
-}
type Rule = String

data RunType = Dead | Alive | Newline
  deriving (Eq)
type Run = (Int, RunType)

data Header = Header Int Int (Maybe Rule)
type RLEData = (Header, [Run])

type Parser s = Parsec s ()

natural :: (Stream s Identity Char) => Parser s Int
natural = do
  num <- many1 digit
  spaces
  return (read num)

symbol :: (Stream s Identity Char) => String -> Parser s ()
symbol sym = string sym >> spaces

parseWidth :: (Stream s Identity Char) => Parser s Int
parseWidth = symbol "x" >> symbol "=" >> natural

parseHeight :: (Stream s Identity Char) => Parser s Int
parseHeight = symbol "," >> symbol "y" >> symbol "=" >> natural

parseRule :: (Stream s Identity Char) => Parser s Rule
parseRule = do
  symbol "," >> symbol "rule" >> symbol "="
  rule <- many1 (satisfy (not . Char.isSpace))
  spaces
  return rule

parseHeader :: (Stream s Identity Char) => Parser s Header
parseHeader = Ap.liftA3 Header parseWidth parseHeight maybeRule
  where maybeRule = fmap Just parseRule <|> return Nothing

parseRunType :: (Stream s Identity Char) => Parser s RunType
parseRunType = (symbol "b" >> return Dead)
  <|> (symbol "o" >> return Alive)
  <|> (symbol "$" >> return Newline)

parseRun :: (Stream s Identity Char) => Parser s Run
parseRun = Ap.liftA2 (,) (natural <|> return 1) parseRunType

parseData :: (Stream s Identity Char) => Parser s RLEData
parseData = do
  rle <- Ap.liftA2 (,) parseHeader (many1 parseRun)
  symbol "!"
  return rle

parseComment :: (Stream s Identity Char) => Parser s ()
parseComment = symbol "#" >> manyTill anyChar newline >> return ()
  where newline = try (symbol "\n")

parseRLE :: (Stream s Identity Char) => Parser s RLEData
parseRLE = spaces >> many parseComment >> parseData

updateRows :: [Run] -> [[Pat.Cell]] -> [[Pat.Cell]]
updateRows = curry \case
  ([], rows) -> rows
  ((0, _) : runs, rows) -> updateRows runs rows

  ((n, rt) : runs, rows) -> let
    add cell = \case
      [] -> [[cell]]
      (row : rest) -> (cell : row) : rest

    runs' = (n - 1, rt) : runs

    rows' = case rt of
      Dead -> add Pat.Dead rows
      Alive -> add Pat.Alive rows
      Newline -> [] : rows

    in updateRows runs' rows'

toPattern :: RLEData -> (Maybe Rule, Pattern)
toPattern (Header h w rule, runs) = let
  rows = updateRows (reverse runs) []
  pat = Pat.setDimensions h w (Pat.fromList rows)
  in (rule, pat)

{-|
Parse an RLE file, returning a 'Rule' (if it exists) and a 'Pattern'.
The argument can be 'String', 'Text', or any other type with a 'Stream'
instance.

This parser is fairly liberal. Whitespace is allowed everywhere except
in the middle of a number or rulestring, and there need not be a
newline after the header. Also, text after the final @!@ character is
ignored.
-}
parse :: (Stream s Identity Char) => s -> Maybe (Maybe Rule, Pattern)
parse str =
  case runParser parseRLE () "" str of
    Left _ -> Nothing
    Right rle -> Just (toPattern rle)

updateRuns :: [[Pat.Cell]] -> [Run] -> [Run]
updateRuns = let
  add rt = \case
    (n, rt') : runs | rt == rt' -> (n + 1, rt) : runs
    runs -> (1, rt) : runs

  in curry \case
  ([], runs) -> reverse runs
  ([] : rows, runs) -> updateRuns rows (add Newline runs)
  ((Pat.Dead : row) : rows, runs) -> updateRuns (row : rows) (add Dead runs)
  ((Pat.Alive : row) : rows, runs) -> updateRuns (row : rows) (add Alive runs)

fromShow :: (Show a, IsString s) => a -> s
fromShow = String.fromString . show

showRunType :: (IsString s) => RunType -> s
showRunType = \case
  Dead -> "b"
  Alive -> "o"
  Newline -> "$"

showRun :: (Monoid s, IsString s) => Run -> s
showRun = \case
  (1, rt) -> showRunType rt
  (n, rt) -> fromShow n <> showRunType rt

showRuns :: (Monoid s, IsString s) => Int -> [Run] -> s
showRuns = curry \case
  (_, []) -> ""
  (0, runs) -> "\n" <> showRuns 30 runs
  (n, run : runs) -> showRun run <> showRuns (n - 1) runs

{-|
Convert a 'Pattern' into an RLE. If the first argument is 'Nothing',
the generated RLE will have no "rule" field in its header.
-}
make :: (Monoid s, IsString s) => Maybe Rule -> Pattern -> s
make rule pat = let
  x = "x = " <> fromShow (Pat.width pat)
  y = ", y = " <> fromShow (Pat.height pat)
  r = case rule of
    Nothing -> ""
    Just str -> ", rule = " <> String.fromString str
  runs = updateRuns (Pat.toList pat) []
  in x <> y <> r <> "\n" <> showRuns 30 runs <> "!\n"

{-|
Convert a list of patterns into RLEs and print them. Example:

> import qualified Text.RLE as RLE
> import Data.CA.List (withDimensions)
> main = RLE.printAll (Just "B3/S23") (withDimensions 4 4)

The program above will print the RLE of every possible pattern
contained in a 4 by 4 square. This data can then be piped into another
program such as apgsearch.
-}
printAll :: Maybe Rule -> [Pattern] -> IO ()
printAll rule = mapM_ (IO.putStrLn . make rule)
