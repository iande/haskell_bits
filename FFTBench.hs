module Main where

import FFT
import Criterion.Config
import Criterion.Main
import Data.Complex

runFFT :: Integer -> [Complex Double] -> [Complex Double]
runFFT sz inps = let theta = (2.0 * pi / fromIntegral(sz))
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

main = do
  putStr "Square Wave Result:\n\t"
  putStrLn . show $ runFFT 16 squareWave
  putStr "Saw-Tooth Wave Result:\n\t"
  putStrLn . show $ runFFT 16 sawToothWave
  defaultMainWith benchConfig (return ()) [
        bgroup "fft" [ bench "square wave" $ nf (runFFT 16) squareWave,
                       bench "saw-tooth wave" $ nf (runFFT 16) sawToothWave
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
	[  8.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	   0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	 (-8.0) :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	   0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0]
Saw-Tooth Wave Result:
	[ 24.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	 (-8.0) :+ (-8.0), 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	   8.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
	(-7.999999999999999) :+ 8.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0]
warming up
estimating clock resolution...
mean is 7.033964 us (80001 iterations)
found 3086 outliers among 79999 samples (3.9%)
  2291 (2.9%) high severe
estimating cost of a clock call...
mean is 60.41778 ns (35 iterations)
found 1 outliers among 35 samples (2.9%)
  1 (2.9%) high mild

benchmarking fft/square wave
collecting 1000 samples, 204 iterations each, in estimated 7.065151 s
mean: 34.04159 us, lb 33.99916 us, ub 34.09050 us, ci 0.950
std dev: 732.3244 ns, lb 648.2252 ns, ub 986.0652 ns, ci 0.950

benchmarking fft/saw-tooth wave
collecting 1000 samples, 210 iterations each, in estimated 7.060356 s
mean: 34.17536 us, lb 34.12706 us, ub 34.23696 us, ci 0.950
std dev: 875.0766 ns, lb 731.1756 ns, ub 1.180891 us, ci 0.950
--}