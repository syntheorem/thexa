module Main where

import System.IO qualified as IO

import Thexa.Internal.DFA qualified as DFA
import Thexa.Internal.Regex.Compiler (compileRegex)
import Thexa.Internal.Unicode.Grapheme (grapheme)

main :: IO ()
main = do
  dfa <- evaluate $ force $ DFA.fromNFA @(DFA.Sparse Word16) $ compileRegex grapheme
  IO.putStrLn "compiled DFA"

  IO.print (DFA.computeStats dfa)
  -- IO.writeFile "graphemeDFA.txt" (DFA.prettyPrint dfa)
