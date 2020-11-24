{-# OPTIONS_GHC -Wno-orphans #-}
module Thexa.Regex.RangeSetSpec where

import PreludePrime

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Thexa.Regex.RangeSet (RangeSet, Range(..))
import Thexa.Regex.RangeSet qualified as RS

rangeGen :: Gen Range
rangeGen = Range <$> arbitrary <*> arbitrary

nonEmptyRangeGen :: Gen Range
nonEmptyRangeGen = do
  l <- arbitrary
  u <- suchThat arbitrary (>= l)
  pure (Range l u)

instance Arbitrary RangeSet where
  arbitrary = RS.unsafeFromList <$> rangeListGen minBound
    where
      rangeListGen c = oneof
        [ pure []
        , do l <- chooseEnum (c, maxBound)
             u <- chooseEnum (l, maxBound)
             if u >= pred maxBound
               then pure [Range l u]
               else (Range l u :) <$> rangeListGen (succ (succ u))
        ]

validRangeSet :: RangeSet -> Bool
validRangeSet set = case RS.toList set of
  [] -> True
  (Range l u : rs) -> l <= u && go u rs
  where
    go _ [] = True
    go b (Range l u : rs) = and @[]
      [ b < maxBound
      , succ b < l
      , l <= u
      , go u rs
      ]

allBounds :: (Char -> Bool) -> RangeSet -> Bool
allBounds p rs = all p' (RS.toList rs)
  where p' (Range l u) = p l && p u

spec :: Spec
spec = do
  describe "union" do
    specify "non-overlapping asc" $
      RS.union (RS.range 'a' 'b') (RS.range 'x' 'z')
        `shouldBe` RS.unsafeFromList [Range 'a' 'b', Range 'x' 'z']

    specify "non-overlapping desc" $
      RS.union (RS.range 'x' 'z') (RS.range 'a' 'b')
        `shouldBe` RS.unsafeFromList [Range 'a' 'b', Range 'x' 'z']

    specify "overlap left" $
      RS.union (RS.range 'a' 'm') (RS.range 'g' 'z')
        `shouldBe` RS.unsafeFromList [Range 'a' 'z']

    specify "overlap right" $
      RS.union (RS.range 'g' 'z') (RS.range 'a' 'm')
        `shouldBe` RS.unsafeFromList [Range 'a' 'z']

    specify "edge overlap left" $
      RS.union (RS.range 'a' 'm') (RS.range 'n' 'z')
        `shouldBe` RS.unsafeFromList [Range 'a' 'z']

    specify "edge overlap right" $
      RS.union (RS.range 'n' 'z') (RS.range 'a' 'm')
        `shouldBe` RS.unsafeFromList [Range 'a' 'z']

    specify "contained left" $
      RS.union (RS.range 'a' 'z') (RS.range 'n' 'x')
        `shouldBe` RS.unsafeFromList [Range 'a' 'z']

    specify "contained right" $
      RS.union (RS.range 'n' 'x') (RS.range 'a' 'z')
        `shouldBe` RS.unsafeFromList [Range 'a' 'z']

    specify "complex" $
      foldMap @[] RS.unsafeFromList [ [Range '6' '9', Range 'a' 'c']
                                    , [Range '0' '5']
                                    , [Range '1' '3', Range 'a' 'd', Range 'A' 'Z']
                                    ]
        `shouldBe` RS.unsafeFromList [Range '0' '9', Range 'a' 'd', Range 'A' 'Z']

    prop "union full x == full" $
      \x -> RS.union RS.full x === RS.full

    prop "union x full == full" $
      \x -> RS.union x RS.full === RS.full

    prop "(union x y) is valid" $
      \x y -> validRangeSet (RS.union x y)

  describe "difference" do
    specify "non-overlapping asc" $
      RS.difference (RS.range 'a' 'b') (RS.range 'x' 'z')
        `shouldBe` RS.unsafeFromList [Range 'a' 'b']

    specify "non-overlapping desc" $
      RS.difference (RS.range 'x' 'z') (RS.range 'a' 'b')
        `shouldBe` RS.unsafeFromList [Range 'x' 'z']

    specify "overlap left" $
      RS.difference (RS.range 'a' 'm') (RS.range 'g' 'z')
        `shouldBe` RS.unsafeFromList [Range 'a' 'f']

    specify "overlap right" $
      RS.difference (RS.range 'g' 'z') (RS.range 'a' 'm')
        `shouldBe` RS.unsafeFromList [Range 'n' 'z']

    specify "contained left" $
      RS.difference (RS.range 'a' 'z') (RS.range 'n' 'x')
        `shouldBe` RS.unsafeFromList [Range 'a' 'm', Range 'y' 'z']

    specify "contained right" $
      RS.difference (RS.range 'n' 'x') (RS.range 'a' 'z')
        `shouldBe` RS.unsafeFromList []

    specify "complement" $
      RS.complement (RS.range 'b' 'y')
        `shouldBe` RS.unsafeFromList [Range minBound 'a', Range 'z' maxBound]

    prop "difference (union x y) y == difference x y" $
      \x y -> RS.difference (RS.union x y) y === RS.difference x y

    prop "difference x x == empty" $
      \x -> RS.difference x x === RS.empty

    prop "difference x full == empty" $
      \x -> RS.difference x RS.full === RS.empty

    prop "(difference x y) is valid" $
      \x y -> validRangeSet (RS.difference x y)

  describe "splitLE" do
    prop "uncurry union . splitLE c == id" $
      \c rs -> uncurry RS.union (RS.splitLE c rs) === rs

    prop "makes valid sets" $
      \c rs -> let (l, r) = RS.splitLE c rs in validRangeSet l && validRangeSet r

    prop "satisfies split" $
      \c rs -> let (l, r) = RS.splitLE c rs in allBounds (<= c) l && allBounds (> c) r
