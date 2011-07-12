module Main where

import FFT
import Criterion.Config
import Criterion.Main
import Data.Complex

--runFFT :: (Integer -> [Complex Double] -> [Complex Double]) -> Integer -> [Complex Double] -> [Complex Double]
runFFT f sz inps = let theta = (2.0 * pi / fromIntegral(sz))
                       w     = conjugate $ cis theta
                       in f sz w inps

squareWave :: [Complex Double]
squareWave = [0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0,
              0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0]

sawToothWave :: [Complex Double]
sawToothWave = [1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0, 1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0,
                1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0, 1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0]

benchConfig = defaultConfig {
                cfgSamples = ljust 1000
              }

main = do
  putStr "Square Wave Result:\n\t"
  putStrLn . show $ runFFT fft 16 squareWave
  putStr "Saw-Tooth Wave Result:\n\t"
  putStrLn . show $ runFFT fft 16 sawToothWave
  defaultMainWith benchConfig (return ()) [
        bgroup "fft" [ bench "square wave" $ nf (runFFT fft 16) squareWave,
                       bench "saw-tooth wave" $ nf (runFFT fft 16) sawToothWave
                     ]
       ]

{--
Results shown below come from compiling and running the benchmarks:

      ghc -o fftbench FFTBench.hs
      ./fftbench

I have also formatted the resulting lists to make them a little prettier
--}

{--
Square Wave Result:
	[8.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	 (-8.0) :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0]
Saw-Tooth Wave Result:
	[24.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	(-8.0) :+ (-8.0), 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	8.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	(-7.999999999999999) :+ 8.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0]
warming up
estimating clock resolution...
mean is 6.990687 us (80001 iterations)
found 2346 outliers among 79999 samples (2.9%)
  1755 (2.2%) high severe
estimating cost of a clock call...
mean is 59.62360 ns (36 iterations)
found 1 outliers among 36 samples (2.8%)
  1 (2.8%) high mild

benchmarking fft/square wave
collecting 1000 samples, 232 iterations each, in estimated 7.019795 s
mean: 30.44903 us, lb 30.42474 us, ub 30.47662 us, ci 0.950
std dev: 415.8990 ns, lb 372.7239 ns, ub 479.0942 ns, ci 0.950

benchmarking fft/saw-tooth wave
collecting 1000 samples, 231 iterations each, in estimated 6.998573 s
mean: 30.50131 us, lb 30.46343 us, ub 30.56316 us, ci 0.950
std dev: 763.3206 ns, lb 552.3002 ns, ub 1.337548 us, ci 0.950
--}