{-# LANGUAGE BlockArguments #-}

module Text.RLESpec (spec) where

import Test.Hspec (Spec, it, describe, shouldBe)

import qualified Text.RLE as RLE
import TestPatterns

spec :: Spec
spec = do
  describe "Text.RLE.parse" do
    it "parses the empty pattern" do
      RLE.parse "x=0,y=0!" `shouldBe` Just (Nothing, empty)
    it "sets the dimensions of a pattern" do
      RLE.parse "x=3,y=2!" `shouldBe` Just (Nothing, allDead)
      RLE.parse "x=5,y=4 bo$2bo$3o!" `shouldBe` Just (Nothing, largeGlider)
      RLE.parse "x=4,y=2 bo$2bo$3o!" `shouldBe` Just (Nothing, cutGlider)
    it "parses gliders" do
      RLE.parse "x=3,y=3 bo$2bo$3o!" `shouldBe` Just (Nothing, glider)
      RLE.parse "x=3,y=3 bo$bbo$ooo!" `shouldBe` Just (Nothing, glider)
      RLE.parse "x=3,y=3 bob$2bo$3o$$!" `shouldBe` Just (Nothing, glider)
    it "tolerates whitespace" do
      RLE.parse " x = 0 \n , y = 0 \n ! " `shouldBe` Just (Nothing, empty)
      RLE.parse "x=3,y=3 bo b$2\nbo$ 3o\n!" `shouldBe` Just (Nothing, glider)
    it "ignores characters after the !" do
      RLE.parse "x=0,y=0!hello, world!" `shouldBe` Just (Nothing, empty)
    it "parses comments" do
      RLE.parse " #a\n# bcd \nx=0,y=0!" `shouldBe` Just (Nothing, empty)
    it "parses rulestrings" do
      RLE.parse "x=0,y=0,rule=b2ak3-jnqr4ce/s23 !"
        `shouldBe` Just (Just "b2ak3-jnqr4ce/s23", empty)
      RLE.parse "x=3,y=3,rule=b3/s23 bo$2bo$3o!"
        `shouldBe` Just (Just "b3/s23", glider)

  describe "Text.RLE.parseMany" do
    it "parses no patterns" do
      RLE.parseMany "" `shouldBe` Just []
    it "parses one pattern" do
      RLE.parseMany "x=3,y=3 bo$2bo$3o!" `shouldBe` Just [(Nothing, glider)]
    it "parses multiple patterns and ignores characters after the last !" do
      RLE.parseMany ("#empty \nx=0,y=0!" ++
        "#glider \nx=3,y=3,rule=b3/s23 bo$2bo$3o!" ++
        "#diagonal \nx=4,y=3 o$bo$2bo! ignore")
        `shouldBe` Just
          [ (Nothing, empty)
          , (Just "b3/s23", glider)
          , (Nothing, diagonal)
          ]

  describe "Text.RLE.make" do
    it "converts patterns into RLEs" do
      RLE.make Nothing empty `shouldBe` "x = 0, y = 0\n!\n"
      RLE.make Nothing glider `shouldBe` "x = 3, y = 3\nbob$2bo$3o$!\n"
      RLE.make Nothing diagonal `shouldBe` "x = 4, y = 3\no3b$bo2b$2bob$!\n"
    it "allows rulestrings" do
      RLE.make (Just "b3/s23") glider
        `shouldBe` "x = 3, y = 3, rule = b3/s23\nbob$2bo$3o$!\n"
