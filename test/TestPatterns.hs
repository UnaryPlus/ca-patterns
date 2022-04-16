{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

module TestPatterns where

import Data.String (IsString)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.CA.Pattern as Pat
import Data.CA.Pattern (Pattern, Cell)

o :: Cell
o = False

pattern O :: Cell
pattern O = True

empty :: Pattern
empty = Pat.fromList []

singleton :: Pattern
singleton = Pat.fromList [ [ O ] ]

allDead :: Pattern
allDead = Pat.fromList
  [ [ o, o, o ]
  , [ o, o, o ]
  ]

glider :: Pattern
glider = Pat.fromList
  [ [ o, O, o ]
  , [ o, o, O ]
  , [ O, O, O ]
  ]

largeGlider :: Pattern
largeGlider = Pat.fromList
  [ [ o, O, o, o, o ]
  , [ o, o, O, o, o ]
  , [ O, O, O, o, o ]
  , [ o, o, o, o, o ]
  ]

cutGlider :: Pattern
cutGlider = Pat.fromList
  [ [ o, O, o, o ]
  , [ o, o, O, o ]
  ]

twoGliders1 :: Pattern
twoGliders1 = Pat.fromList
  [ [ o, O, o, o, o ]
  , [ o, o, O, o, o ]
  , [ O, O, O, o, o ]
  , [ o, o, o, O, o ]
  , [ o, o, o, o, O ]
  , [ o, o, O, O, O ]
  ]

twoGliders2 :: Pattern
twoGliders2 = Pat.fromList
  [ [ o, o, o, O, o ]
  , [ o, o, o, o, O ]
  , [ o, o, O, O, O ]
  , [ o, O, o, o, o ]
  , [ o, o, O, o, o ]
  , [ O, O, O, o, o ]
  ]

gliderStr1, gliderStr2 :: (IsString s) => s
gliderStr1 = ".O.\n..O\nOOO\n"
gliderStr2 = "_*_\n__*\n***\n"

diagonal :: Pattern
diagonal = Pat.fromList
  [ [ O, o, o, o ]
  , [ o, O, o, o ]
  , [ o, o, O, o ]
  ]

reflectedX :: Pattern
reflectedX = Pat.fromList
  [ [ o, o, o, O ]
  , [ o, o, O, o ]
  , [ o, O, o, o ]
  ]

reflectedY :: Pattern
reflectedY = Pat.fromList
  [ [ o, o, O, o ]
  , [ o, O, o, o ]
  , [ O, o, o, o ]
  ]

rotatedL :: Pattern
rotatedL = Pat.fromList
  [ [ o, o, o ]
  , [ o, o, O ]
  , [ o, O, o ]
  , [ O, o, o ]
  ]

rotatedR :: Pattern
rotatedR = Pat.fromList
  [ [ o, o, O ]
  , [ o, O, o ]
  , [ O, o, o ]
  , [ o, o, o ]
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
