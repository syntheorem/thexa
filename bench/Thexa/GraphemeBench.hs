module Thexa.GraphemeBench where

import Criterion

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.IO (FilePath)

import Thexa.Position (findGraphemeBoundary)
import Thexa.Internal.DFA (DFA)
import Thexa.Internal.DFA qualified as DFA
import Thexa.Internal.Regex.Compiler (compileRegex)
import Thexa.Internal.Unicode.Grapheme (grapheme)

-- | Benchmarks for the regex to match a Unicode grapheme cluster.
--
-- These benchmarks serve a few purposes. First, we want to benchmark compiling a regex to a DFA.
-- Second, we want to benchmark executing the various types of DFAs. The grapheme regex is
-- convenient for these purposes because it is non-trivial and actually useful, but is nonetheless
-- relatively simple compared to an entire lexer.
--
-- Third, we want to benchmark findGraphemeBoundary since we're running it as part of the inner loop
-- of the lexer. In particular, we want to see how much the optimizations for ASCII-only text pay
-- off.
benchmarks :: [Benchmark]
benchmarks =
  [ bench "compileRegex" $
      whnf compileRegex grapheme

  , env (pure (compileRegex grapheme)) \nfa ->
    bgroup "DFA.fromNFA"
      [ bench "Dense16"  $ whnf (DFA.fromNFA @DFA.Dense16)  nfa
      , bench "Dense32"  $ whnf (DFA.fromNFA @DFA.Dense32)  nfa
      , bench "Sparse16" $ whnf (DFA.fromNFA @DFA.Sparse16) nfa
      , bench "Sparse32" $ whnf (DFA.fromNFA @DFA.Sparse32) nfa
      ]

  , env (pure graphemeDFAs) \dfas ->
    bgroup "countGraphemes"
      [ benchCountGraphemes "ASCII.txt" dfas
      , benchCountGraphemes "Unicode.txt" dfas
      , benchCountGraphemes "UnicodeNFD.txt" dfas
      ]
  ]

benchCountGraphemes :: FilePath -> GraphemeDFAs -> Benchmark
benchCountGraphemes fileName ~(dense16, dense32, sparse16, sparse32) =
  env (BS.readFile ("bench/"<>fileName)) \bs ->
  bgroup fileName
    [ bench "Dense16"  $ whnf (countGraphemes (dfaMatchLength dense16))  bs
    , bench "Dense32"  $ whnf (countGraphemes (dfaMatchLength dense32))  bs
    , bench "Sparse16" $ whnf (countGraphemes (dfaMatchLength sparse16)) bs
    , bench "Sparse32" $ whnf (countGraphemes (dfaMatchLength sparse32)) bs

    , bench "findGraphemeBoundary" $ whnf (countGraphemes (findGraphemeBoundary BS.uncons)) bs
    ]

{-# INLINE countGraphemes #-}
countGraphemes :: (ByteString -> Int) -> ByteString -> Int
countGraphemes findBoundary = go 0
  where
    go !i !bs =
      let b = findBoundary bs in
      if b == 0 then i else go (i + 1) (BS.drop b bs)

dfaMatchLength :: DFA.Transitions t => DFA.DFA t -> ByteString -> Int
dfaMatchLength dfa = go 0 0 DFA.startNode
  where
    go !i !lastOffset node bs
      | Just (b, bs') <- BS.uncons bs
      , Just node' <- DFA.step dfa node b =
          let lastOffset' = if DFA.isMatchNode dfa node' then i + 1 else lastOffset
          in go (i + 1) lastOffset' node' bs'
      | otherwise = lastOffset

type GraphemeDFAs =
  ( DFA DFA.Dense16
  , DFA DFA.Dense32
  , DFA DFA.Sparse16
  , DFA DFA.Sparse32
  )

graphemeDFAs :: GraphemeDFAs
graphemeDFAs =
  ( DFA.fromNFA @DFA.Dense16  nfa
  , DFA.fromNFA @DFA.Dense32  nfa
  , DFA.fromNFA @DFA.Sparse16 nfa
  , DFA.fromNFA @DFA.Sparse32 nfa
  )
  where nfa = compileRegex grapheme
