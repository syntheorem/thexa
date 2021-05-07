module Thexa.PositionSpec where

import PreludePrime
import Test.Hspec

import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.List (zip)
import Thexa.Internal.Unicode.Parser (readGraphemeBreakTest)
import Thexa.Position (findGraphemeBoundary)

graphemeList :: [[String]]
graphemeList = $$(readGraphemeBreakTest)

spec :: Spec
spec = do
  describe "findGraphemeBoundary" do
    for_ (zip [(0::Int)..] graphemeList) \(i, gs) -> do
      specify ("test case "<>show i) (testGraphemes gs)

testGraphemes :: [String] -> Expectation
testGraphemes gs = go 0 str lengths
  where
    str = UTF8.fromString (fold gs)
    lengths = map (BS.length . UTF8.fromString) gs

    go off bs (l:ls) = do
      let boundary = findGraphemeBoundary BS.uncons bs
      when (boundary /= l) do
        expectationFailure $ "offset: "<>show off<>
          ", expected: "<>show l<>", found: "<>show boundary
      go (off + l) (BS.drop l bs) ls

    go off bs []
      | BS.null bs = pure ()
      | otherwise  = expectationFailure $ "test bug, offset: "<>show off
