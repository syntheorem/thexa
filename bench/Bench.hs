import PreludePrime
import Criterion.Main

import qualified Thexa.PrimMapBench

main :: IO ()
main = defaultMain
  [ bgroup "Thexa.PrimMap" Thexa.PrimMapBench.benchmarks
  ]
