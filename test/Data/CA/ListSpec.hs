{-# LANGUAGE BlockArguments #-}

module Data.CA.ListSpec (spec) where

import Test.Hspec (Spec, it, describe, shouldBe)

import Data.CA.List (withDimensions, combinations)
import TestPatterns

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
