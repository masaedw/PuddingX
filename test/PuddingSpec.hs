{-# LANGUAGE OverloadedStrings #-}
module PuddingSpec where

import Test.Hspec
import Pudding

spec :: Spec
spec = do
  describe "cthen" $ do
    it "can compile if .. then" $
       cthen undefined [PNumber 1, PNumber 2, PWord "if", PBool True] `shouldBe`
           Right [PNumber 1, PNumber 2, PWord "jump", PNumber 2, PBool True]
    it "can compile if .. else .. then" $
       cthen undefined [PNumber 1, PWord "else", PNumber 2, PWord "if", PBool True] `shouldBe`
           Right [PNumber 1, PWord "fjump", PNumber 1, PNumber 2, PWord "jump", PNumber 3, PBool True]
