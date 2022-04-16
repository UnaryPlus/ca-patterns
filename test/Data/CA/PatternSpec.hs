{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Data.CA.PatternSpec (spec) where

import qualified Data.Vector as Vec

import Test.Hspec (Spec, it, describe, shouldBe)

import qualified Data.CA.Pattern as Pat
import TestPatterns

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
    it "does nothing to the empty pattern" do
      Pat.trimTop empty `shouldBe` empty

  describe "Data.CA.Pattern.trimBottom" do
    it "removes rows of dead cells from the bottom" do
      Pat.trimBottom untrimmed `shouldBe` trimmedBottom
    it "is idempotent" do
      Pat.trimBottom trimmedBottom `shouldBe` trimmedBottom
    it "does nothing to the empty pattern" do
      Pat.trimBottom empty `shouldBe` empty

  describe "Data.CA.Pattern.trimLeft" do
    it "removes columns of dead cells from the left" do
      Pat.trimLeft untrimmed `shouldBe` trimmedLeft
    it "is idempotent" do
      Pat.trimLeft trimmedLeft `shouldBe` trimmedLeft
    it "does nothing to the empty pattern" do
      Pat.trimLeft empty `shouldBe` empty

  describe "Data.CA.Pattern.trimRight" do
    it "removes columns of dead cells from the right" do
      Pat.trimRight untrimmed `shouldBe` trimmedRight
    it "is idempotent" do
      Pat.trimRight trimmedRight `shouldBe` trimmedRight
    it "does nothing to the empty pattern" do
      Pat.trimRight empty `shouldBe` empty

  describe "Data.CA.Pattern.trim" do
    it "removes all layers of dead cells" do
      Pat.trim untrimmed `shouldBe` singleton
    it "is idempotent" do
      Pat.trim singleton `shouldBe` singleton
    it "does nothing to the empty pattern" do
      Pat.trim empty `shouldBe` empty

  describe "Data.CA.Pattern.setHeight" do
    it "adds rows of dead cells to the bottom" do
      Pat.setHeight 5 trimmedBottom `shouldBe` untrimmed
    it "removes rows from the bottom" do
      Pat.setHeight 3 untrimmed `shouldBe` trimmedBottom

  describe "Data.CA.Pattern.setWidth" do
    it "adds columns of dead cells to the right" do
      Pat.setWidth 5 trimmedRight `shouldBe` untrimmed
    it "removes columns from the right" do
      Pat.setWidth 3 untrimmed `shouldBe` trimmedRight

  describe "Data.CA.Pattern.setDimensions" do
    it "sets the height and width of a pattern" do
      Pat.setDimensions 4 5 glider `shouldBe` largeGlider
      Pat.setDimensions 2 4 glider `shouldBe` cutGlider
      Pat.setDimensions 2 3 empty `shouldBe` allDead

  describe "Data.CA.Pattern.reflectX" do
    it "reflects a pattern horizontally" do
      Pat.reflectX diagonal `shouldBe` reflectedX
    it "does nothing to the empty pattern" do
      Pat.reflectX empty `shouldBe` empty

  describe "Data.CA.Pattern.reflectY" do
    it "reflects a pattern vertically" do
      Pat.reflectY diagonal `shouldBe` reflectedY
    it "does nothing to the empty pattern" do
      Pat.reflectY empty `shouldBe` empty

  describe "Data.CA.Pattern.rotateL" do
    it "rotates a pattern counterclockwise" do
      Pat.rotateL diagonal `shouldBe` rotatedL
    it "does nothing to the empty pattern" do
      Pat.rotateL empty `shouldBe` empty

  describe "Data.CA.Pattern.rotateR" do
    it "rotates a pattern clockwise" do
      Pat.rotateR diagonal `shouldBe` rotatedR
    it "does nothing to the empty pattern" do
      Pat.rotateR empty `shouldBe` empty

  describe "Data.CA.Pattern.combine" do
    it "combines two patterns" do
      Pat.combine 3 2 glider glider `shouldBe` twoGliders1
      Pat.combine (-3) (-2) glider glider `shouldBe` twoGliders1
      Pat.combine 3 (-2) glider glider `shouldBe` twoGliders2
      Pat.combine (-3) 2 glider glider `shouldBe` twoGliders2
