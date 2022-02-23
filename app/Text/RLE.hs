{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, FlexibleContexts #-}

module Text.RLE (parse, make, printAll) where

import qualified Data.Char as Char
import qualified Data.String as String
import qualified Control.Applicative as Ap
import Data.Functor.Identity (Identity)
import Data.String (IsString)

import Text.Parsec
  ( Parsec, Stream, runParser
  , (<|>), many, satisfy
  , digit, string, spaces, eof
  )

import qualified Data.Text.IO as IO

import qualified Data.CA.Pattern as Pat
import Data.CA.Pattern (Pattern)

type Rule = String

data RunType = Dead | Alive | Newline
  deriving (Eq)
type Run = (Int, RunType)

data Header = Header Int Int (Maybe Rule)
type RLEData = (Header, [Run])

type Parser s = Parsec s ()

natural :: (Stream s Identity Char) => Parser s Int
natural = fmap read (many digit) <* spaces

symbol :: (Stream s Identity Char) => String -> Parser s ()
symbol sym = string sym >> spaces

parseWidth :: (Stream s Identity Char) => Parser s Int
parseWidth = symbol "x" >> symbol "=" >> natural

parseHeight :: (Stream s Identity Char) => Parser s Int
parseHeight = symbol "," >> symbol "y" >> symbol "=" >> natural

parseRule :: (Stream s Identity Char) => Parser s Rule
parseRule = symbol "," >> symbol "rule" >> symbol "="
  >> many (satisfy (not . Char.isSpace))

parseHeader :: (Stream s Identity Char) => Parser s Header
parseHeader = Ap.liftA3 Header parseWidth parseHeight maybeRule
  where maybeRule = fmap Just parseRule <|> return Nothing

parseRunType :: (Stream s Identity Char) => Parser s RunType
parseRunType = (symbol "b" >> return Dead)
  <|> (symbol "o" >> return Alive)
  <|> (symbol "$" >> return Newline)

parseRun :: (Stream s Identity Char) => Parser s Run
parseRun = Ap.liftA2 (,) (natural <|> return 1) parseRunType

parseRLE :: (Stream s Identity Char) => Parser s RLEData
parseRLE = Ap.liftA2 (,) parseHeader (many parseRun) <* (symbol "!" >> eof)

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
  rows = updateRows runs []
  pat = Pat.setDimensions h w (Pat.fromList rows)
  in (rule, pat)

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
  ([], runs) -> runs
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

showRun :: (Semigroup s, IsString s) => Run -> s
showRun = \case
  (1, rt) -> showRunType rt
  (n, rt) -> fromShow n <> showRunType rt

make :: (Monoid s, IsString s) => Maybe Rule -> Pattern -> s
make rule pat = let
  x = "x = " <> fromShow (Pat.width pat)
  y = ", y = " <> fromShow (Pat.height pat)
  r = case rule of
    Nothing -> ""
    Just str -> ", rule = " <> String.fromString str
  runs = updateRuns (Pat.toList pat) []
  in x <> y <> r <> "\n" <> foldMap showRun runs <> "!\n"

printAll :: Maybe Rule -> [Pattern] -> IO ()
printAll rule = mapM_ (IO.putStrLn . make rule)
