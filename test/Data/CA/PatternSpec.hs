{-# LANGUAGE BlockArguments, OverloadedStrings, PatternSynonyms #-}

module Data.CA.PatternSpec (spec) where

import Data.String (IsString)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Test.Hspec (Spec, it, describe, shouldBe)

import qualified Data.CA.Pattern as Pat
import Data.CA.Pattern (Pattern, Cell)

o :: Cell
o = False

pattern O :: Cell
pattern O = True

empty :: Pattern
empty = Pat.fromList []

glider :: Pattern
glider = Pat.fromList
  [ [ o, O, o ]
  , [ o, o, O ]
  , [ O, O, O ]
  ]

gliderStr1, gliderStr2 :: (IsString s => s)
gliderStr1 = ".O.\n..O\nOOO\n"
gliderStr2 = "_*_\n__*\n***\n"

diagonal :: Pattern
diagonal = Pat.fromList
  [ [ O, o, o, o ]
  , [ o, O, o, o ]
  , [ o, o, O, o ]
  ]

horizontal :: Pattern
horizontal = Pat.fromList
  [ [ O, O, O ]
  , [ o, o, o ]
  , [ O, O, O ]
  , [ o, o, o ]
  , [ O, O, O ]
  ]

list1 :: [[Cell]]
list1 =
  [ [ o, O, o, O ]
  , [ O, o, o ]
  , [ o, O, o ]
  ]

list2 :: [[Cell]]
list2 =
  [ [ o, o, O ]
  , [ o, O, o ]
  , []
  ]

vector1, vector2 :: Vector (Vector Cell)
vector1 = Vec.fromList (map Vec.fromList list1)
vector2 = Vec.fromList (map Vec.fromList list2)

fromRectList1, fromRectList2 :: Pattern
fromRectList1 = Pat.fromRectList list1
fromRectList2 = Pat.fromRectList list2

fromList1, fromList2 :: Pattern
fromList1 = Pat.fromList list1
fromList2 = Pat.fromList list2

fromRectVector1, fromRectVector2 :: Pattern
fromRectVector1 = Pat.fromRectVector vector1
fromRectVector2 = Pat.fromRectVector vector2

fromVector1, fromVector2 :: Pattern
fromVector1 = Pat.fromVector vector1
fromVector2 = Pat.fromVector vector2

paddedList1 :: [[Cell]]
paddedList1 =
  [ [ o, O, o, O ]
  , [ O, o, o, o ]
  , [ o, O, o, o ]
  ]

paddedList2 :: [[Cell]]
paddedList2 =
  [ [ o, o, O ]
  , [ o, O, o ]
  , [ o, o, o ]
  ]

paddedVector1, paddedVector2 :: Vector (Vector Cell)
paddedVector1 = Vec.fromList (map Vec.fromList paddedList1)
paddedVector2 = Vec.fromList (map Vec.fromList paddedList2)

untrimmed :: Pattern
untrimmed = Pat.fromList
  [ [ o, o, o, o, o ]
  , [ o, o, o, o, o ]
  , [ o, o, O, o, o ]
  , [ o, o, o, o, o ]
  , [ o, o, o, o, o ]
  ]

trimmedTop :: Pattern
trimmedTop = Pat.fromList
  [ [ o, o, O, o, o ]
  , [ o, o, o, o, o ]
  , [ o, o, o, o, o ]
  ]

trimmedBottom :: Pattern
trimmedBottom = Pat.fromList
  [ [ o, o, o, o, o ]
  , [ o, o, o, o, o ]
  , [ o, o, O, o, o ]
  ]

trimmedLeft :: Pattern
trimmedLeft = Pat.fromList
  [ [ o, o, o ]
  , [ o, o, o ]
  , [ O, o, o ]
  , [ o, o, o ]
  , [ o, o, o ]
  ]

trimmedRight :: Pattern
trimmedRight = Pat.fromList
  [ [ o, o, o ]
  , [ o, o, o ]
  , [ o, o, O ]
  , [ o, o, o ]
  , [ o, o, o ]
  ]

trimmedAll :: Pattern
trimmedAll = Pat.fromList
  [ [ O ] ]

spec :: Spec
spec = do
  describe "Prelude.show" do
    it "shows a pattern's list structure" do
      show glider `shouldBe` ("fromList " ++
        "[[False,True,False],[False,False,True],[True,True,True]]")

  describe "Data.CA.Pattern.lookup" do
    it "returns False for dead cells" do
      Pat.lookup 0 0 glider `shouldBe` False
      Pat.lookup 0 2 glider `shouldBe` False
    it "returns True for live cells" do
      Pat.lookup 2 0 glider `shouldBe` True
      Pat.lookup 2 2 glider `shouldBe` True
    it "returns False when out of bounds" do
      Pat.lookup 4 0 glider `shouldBe` False
      Pat.lookup 0 (-2) glider `shouldBe` False

  describe "Data.CA.Pattern.generate" do
    it "creates diagonal lines" do
      Pat.generate 3 4 (==) `shouldBe` diagonal
    it "creates horizontal stripes" do
      Pat.generate 5 3 (\r _ -> even r) `shouldBe` horizontal

  describe "Data.CA.Pattern.height" do
    it "returns a pattern's height" do
      Pat.height glider `shouldBe` 3
      Pat.height horizontal `shouldBe` 5
      Pat.height empty `shouldBe` 0

  describe "Data.CA.Pattern.width" do
    it "returns a pattern's width" do
      Pat.width glider `shouldBe` 3
      Pat.width horizontal `shouldBe` 3
      Pat.width empty `shouldBe` 0

  describe "Data.CA.Pattern.dimensions" do
    it "returns a pattern's height and width" do
      Pat.dimensions glider `shouldBe` (3, 3)
      Pat.dimensions horizontal `shouldBe` (5, 3)
      Pat.dimensions empty `shouldBe` (0, 0)

  describe "Data.CA.Pattern.valid" do
    it "returns True for rectangular patterns" do
      Pat.valid glider `shouldBe` True
      Pat.valid diagonal `shouldBe` True
      Pat.valid empty `shouldBe` True
    it "returns False for non-rectangular patterns" do
      Pat.valid fromRectList1 `shouldBe` False
      Pat.valid fromRectList2 `shouldBe` False

  describe "Data.CA.Pattern.fromRectList" do
    it "converts lists directly into patterns" do
      Pat.toList fromRectList1 `shouldBe` list1
      Pat.toList fromRectList2 `shouldBe` list2

  describe "Data.CA.Pattern.fromList" do
    it "pads non-rectangular patterns with dead cells" do
      Pat.toList fromList1 `shouldBe` paddedList1
      Pat.toList fromList2 `shouldBe` paddedList2

  describe "Data.CA.Pattern.toList" do
    it "converts patterns into lists" do
      Pat.toList glider `shouldBe`
        [ [ o, O, o ]
        , [ o, o, O ]
        , [ O, O, O ]
        ]

  describe "Data.CA.Pattern.fromRectVector" do
    it "converts vectors directly into patterns" do
      Pat.toVector fromRectVector1 `shouldBe` vector1
      Pat.toVector fromRectVector2 `shouldBe` vector2

  describe "Data.CA.Pattern.fromVector" do
    it "pads non-rectangular patterns with dead cells" do
      Pat.toVector fromVector1 `shouldBe` paddedVector1
      Pat.toVector fromVector2 `shouldBe` paddedVector2

  describe "Data.CA.Pattern.toVector" do
    it "converts patterns into vectors" do
      Pat.toVector glider `shouldBe` Vec.fromList
        [ Vec.fromList [ o, O, o ]
        , Vec.fromList [ o, o, O ]
        , Vec.fromList [ O, O, O ]
        ]

  describe "Data.CA.Pattern.toText" do
    it "converts patterns into text" do
      Pat.toText '.' 'O' glider `shouldBe` gliderStr1
      Pat.toText '_' '*' glider `shouldBe` gliderStr2
      Pat.toText 'v' 'v' empty `shouldBe` ""

  describe "Data.CA.Pattern.toString" do
    it "converts patterns into strings" do
      Pat.toString '.' 'O' glider `shouldBe` gliderStr1
      Pat.toString '_' '*' glider `shouldBe` gliderStr2
      Pat.toString 'v' 'v' empty `shouldBe` ""

  describe "Data.CA.Pattern.trimTop" do
    it "removes rows of dead cells from the top" do
      Pat.trimTop untrimmed `shouldBe` trimmedTop
    it "is idempotent" do
      Pat.trimTop trimmedTop `shouldBe` trimmedTop

  describe "Data.CA.Pattern.trimBottom" do
    it "removes rows of dead cells from the bottom" do
      Pat.trimBottom untrimmed `shouldBe` trimmedBottom
    it "is idempotent" do
      Pat.trimBottom trimmedBottom `shouldBe` trimmedBottom

  describe "Data.CA.Pattern.trimLeft" do
    it "removes columns of dead cells from the left" do
      Pat.trimLeft untrimmed `shouldBe` trimmedLeft
    it "is idempotent" do
      Pat.trimLeft trimmedLeft `shouldBe` trimmedLeft

  describe "Data.CA.Pattern.trimRight" do
    it "removes columns of dead cells from the right" do
      Pat.trimRight untrimmed `shouldBe` trimmedRight
    it "is idempotent" do
      Pat.trimRight trimmedRight `shouldBe` trimmedRight
