module Main where

import PreludePrime

import Data.Compact
import System.IO qualified as IO

import Thexa.Internal.DFA qualified as DFA
import Thexa.Internal.Regex.Compiler (compileRegex)
import Thexa.Internal.Unicode.Grapheme (grapheme)

main :: IO ()
main = do
  dfa <- evaluate $ force $ DFA.fromNFA @(DFA.Dense Word16) $ compileRegex grapheme
  IO.putStrLn "compiled DFA"

  -- compactDFA <- compact dfa
  -- size <- compactSize compactDFA
  -- IO.putStrLn $ "compacted size: "<>show size

  IO.print (DFA.computeStats dfa)
  IO.writeFile "graphemeDFA.txt" (DFA.prettyPrint dfa)
