import PreludePrime
import Criterion.Main

import qualified Thexa.GraphemeBench

main :: IO ()
main = defaultMain
  [ bgroup "Grapheme" Thexa.GraphemeBench.benchmarks
  ]
