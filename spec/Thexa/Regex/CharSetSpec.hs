{-# OPTIONS_GHC -Wno-orphans #-}
module Thexa.Regex.CharSetSpec where

import PreludePrime

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Thexa.Regex.CharSet (CharSet)
import Thexa.Regex.CharSet qualified as CS

instance Arbitrary CharSet where
  arbitrary = CS.unsafeFromList <$> rangeListGen minBound
    where
      rangeListGen c = oneof
        [ pure []
        , do l <- chooseEnum (c, maxBound)
             u <- chooseEnum (l, maxBound)
             if u >= pred maxBound
               then pure [(l, u)]
               else ((l, u) :) <$> rangeListGen (succ (succ u))
        ]

-- Verify that the invariants of a char set hold.
validRangeSet :: CharSet -> Bool
validRangeSet set = case CS.toList set of
  [] -> True
  ((l, u) : rs) -> l <= u && go u rs
  where
    go _ [] = True
    go b ((l, u) : rs) = and
      [ b < maxBound
      , succ b < l
      , l <= u
      , go u rs
      ]

-- Apply a predicate to every bound in a char set.
allBounds :: (Char -> Bool) -> CharSet -> Bool
allBounds p rs = all p' (CS.toList rs)
  where p' (l, u) = p l && p u

spec :: Spec
spec = do
  describe "union" do
    specify "non-overlapping asc" $
      CS.union (CS.range 'a' 'b') (CS.range 'x' 'z')
        `shouldBe` CS.unsafeFromList [('a', 'b'), ('x', 'z')]

    specify "non-overlapping desc" $
      CS.union (CS.range 'x' 'z') (CS.range 'a' 'b')
        `shouldBe` CS.unsafeFromList [('a', 'b'), ('x', 'z')]

    specify "overlap left" $
      CS.union (CS.range 'a' 'm') (CS.range 'g' 'z')
        `shouldBe` CS.unsafeFromList [('a', 'z')]

    specify "overlap right" $
      CS.union (CS.range 'g' 'z') (CS.range 'a' 'm')
        `shouldBe` CS.unsafeFromList [('a', 'z')]

    specify "edge overlap left" $
      CS.union (CS.range 'a' 'm') (CS.range 'n' 'z')
        `shouldBe` CS.unsafeFromList [('a', 'z')]

    specify "edge overlap right" $
      CS.union (CS.range 'n' 'z') (CS.range 'a' 'm')
        `shouldBe` CS.unsafeFromList [('a', 'z')]

    specify "contained left" $
      CS.union (CS.range 'a' 'z') (CS.range 'n' 'x')
        `shouldBe` CS.unsafeFromList [('a', 'z')]

    specify "contained right" $
      CS.union (CS.range 'n' 'x') (CS.range 'a' 'z')
        `shouldBe` CS.unsafeFromList [('a', 'z')]

    specify "complex" $
      foldMap CS.unsafeFromList [ [('6', '9'), ('a', 'c')]
                                , [('0', '5')]
                                , [('1', '3'), ('a', 'd'), ('A', 'Z')]
                                ]
        `shouldBe` CS.unsafeFromList [('0', '9'), ('a', 'd'), ('A', 'Z')]

    prop "union full x == full" $
      \x -> CS.union CS.full x === CS.full

    prop "union x full == full" $
      \x -> CS.union x CS.full === CS.full

    prop "(union x y) is valid" $
      \x y -> validRangeSet (CS.union x y)

  describe "difference" do
    specify "non-overlapping asc" $
      CS.difference (CS.range 'a' 'b') (CS.range 'x' 'z')
        `shouldBe` CS.unsafeFromList [('a', 'b')]

    specify "non-overlapping desc" $
      CS.difference (CS.range 'x' 'z') (CS.range 'a' 'b')
        `shouldBe` CS.unsafeFromList [('x', 'z')]

    specify "overlap left" $
      CS.difference (CS.range 'a' 'm') (CS.range 'g' 'z')
        `shouldBe` CS.unsafeFromList [('a', 'f')]

    specify "overlap right" $
      CS.difference (CS.range 'g' 'z') (CS.range 'a' 'm')
        `shouldBe` CS.unsafeFromList [('n', 'z')]

    specify "contained left" $
      CS.difference (CS.range 'a' 'z') (CS.range 'n' 'x')
        `shouldBe` CS.unsafeFromList [('a', 'm'), ('y', 'z')]

    specify "contained right" $
      CS.difference (CS.range 'n' 'x') (CS.range 'a' 'z')
        `shouldBe` CS.unsafeFromList []

    specify "complement" $
      CS.complement (CS.range 'b' 'y')
        `shouldBe` CS.unsafeFromList [(minBound, 'a'), ('z', maxBound)]

    prop "difference (union x y) y == difference x y" $
      \x y -> CS.difference (CS.union x y) y === CS.difference x y

    prop "difference x x == empty" $
      \x -> CS.difference x x === CS.empty

    prop "difference x full == empty" $
      \x -> CS.difference x CS.full === CS.empty

    prop "(difference x y) is valid" $
      \x y -> validRangeSet (CS.difference x y)

  describe "splitLE" do
    prop "uncurry union . splitLE c == id" $
      \c rs -> uncurry CS.union (CS.splitLE c rs) === rs

    prop "makes valid sets" $
      \c rs -> let (l, r) = CS.splitLE c rs in validRangeSet l && validRangeSet r

    prop "satisfies split" $
      \c rs -> let (l, r) = CS.splitLE c rs in allBounds (<= c) l && allBounds (> c) r