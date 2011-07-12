module FFTBench where

import FFT
import Criterion.Config
import Criterion.Main
import Data.Complex

runFft :: Integer -> [Complex Double] -> [Complex Double]
runFft sz inps = let theta = (2.0 * pi / fromIntegral(sz))
                     w     = conjugate $ cis theta
                     in fft sz w inps []

squareWave :: [Complex Double]
squareWave = [0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0,
              0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0]

sawToothWave :: [Complex Double]
sawToothWave = [1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0, 1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0,
                1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0, 1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0]

benchConfig = defaultConfig {
                cfgSamples = ljust 1000
              }

main = defaultMainWith benchConfig (return ()) [
        bgroup "fft" [ bench "square wave" $ nf (runFft 16) squareWave,
                       bench "saw-tooth wave" $ nf (runFft 16) sawToothWave
                     ]
       ]

{--
warming up
estimating clock resolution...
mean is 7.701010 us (80001 iterations)
found 2521 outliers among 79999 samples (3.2%)
  2169 (2.7%) high severe
estimating cost of a clock call...
mean is 67.06615 ns (76 iterations)
found 12 outliers among 76 samples (15.8%)
  4 (5.3%) high mild
  8 (10.5%) high severe

benchmarking fft/square wave
collecting 1000 samples, 44 iterations each, in estimated 7.774219 s
mean: 181.1056 us, lb 180.4757 us, ub 181.9573 us, ci 0.950
std dev: 11.78474 us, lb 9.528298 us, ub 14.98373 us, ci 0.950

benchmarking fft/saw-tooth wave
collecting 1000 samples, 44 iterations each, in estimated 7.713079 s
mean: 178.9541 us, lb 178.5687 us, ub 179.4427 us, ci 0.950
std dev: 6.982693 us, lb 5.837796 us, ub 8.591811 us, ci 0.950
--}