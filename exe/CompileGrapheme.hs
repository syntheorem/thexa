module Main where

import PreludePrime

import Data.Compact
import System.IO qualified as IO

import Thexa.DFA qualified as DFA
import Thexa.Regex.Compiler (compileRegex)
import Thexa.Regex.Unicode (graphemeDFA)

-- graphemeDFA = $(let dfa = DFA.denseFromNFA (compileRegex grapheme) in [| dfa |])

main :: IO ()
main = do
  -- dfa <- evaluate $ force $ DFA.fromNFA @(DFA.Dense Word16) $ compileRegex grapheme
  -- IO.putStrLn "compiled DFA"

  -- compactDFA <- compact dfa
  -- size <- compactSize compactDFA
  -- IO.putStrLn $ "compacted size: "<>show size

  IO.print (DFA.computeStats graphemeDFA)
  IO.writeFile "graphemeDFA.txt" (DFA.prettyPrint graphemeDFA)
