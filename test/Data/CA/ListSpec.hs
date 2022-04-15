{-# LANGUAGE BlockArguments, PatternSynonyms #-}

module Data.CA.ListSpec (spec) where

import Test.Hspec (Spec, it, describe, shouldBe)

import qualified Data.CA.Pattern as Pat
import Data.CA.Pattern (Pattern, Cell)
import Data.CA.List (withDimensions, combinations)

o :: Cell
o = False

pattern O :: Cell
pattern O = True

empty :: Pattern
empty = Pat.fromList []

singleton :: Pattern
singleton = Pat.fromList [ [ O ] ]

oneByTwo :: [Pattern]
oneByTwo = map Pat.fromList
  [ [ [ o, o ] ]
  , [ [ o, O ] ]
  , [ [ O, o ] ]
  , [ [ O, O ] ]
  ]

twoByTwo :: [Pattern]
twoByTwo = map Pat.fromList
  [ [ [ o, o ], [ o, o ] ]
  , [ [ o, o ], [ o, O ] ]
  , [ [ o, o ], [ O, o ] ]
  , [ [ o, o ], [ O, O ] ]
  , [ [ o, O ], [ o, o ] ]
  , [ [ o, O ], [ o, O ] ]
  , [ [ o, O ], [ O, o ] ]
  , [ [ o, O ], [ O, O ] ]
  , [ [ O, o ], [ o, o ] ]
  , [ [ O, o ], [ o, O ] ]
  , [ [ O, o ], [ O, o ] ]
  , [ [ O, o ], [ O, O ] ]
  , [ [ O, O ], [ o, o ] ]
  , [ [ O, O ], [ o, O ] ]
  , [ [ O, O ], [ O, o ] ]
  , [ [ O, O ], [ O, O ] ]
  ]


singletonCombos :: [Pattern]
singletonCombos = map Pat.fromList
  [ [ [ O, o ]
    , [ o, o ]
    , [ o, O ]
    ]
  , [ [ O, o, o ]
    , [ o, o, o ]
    , [ o, o, O ]
    ]
  , [ [ O, o ]
    , [ o, o ]
    , [ o, o ]
    , [ o, O ]
    ]
  , [ [ O, o, o ]
    , [ o, o, o ]
    , [ o, o, o ]
    , [ o, o, O ]
    ]
  ]

spec :: Spec
spec = do
  describe "Data.CA.List.withDimensions" do
    it "lists every pattern with the given dimensions" do
      withDimensions 0 0 `shouldBe` [empty]
      withDimensions 1 2 `shouldBe` oneByTwo
      withDimensions 2 2 `shouldBe` twoByTwo

  describe "Data.CA.List.combinations" do
    it "combines two patterns in multiple ways" do
      combinations (2, 3) (1, 2) singleton singleton
        `shouldBe` singletonCombos
