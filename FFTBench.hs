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

test1Wave :: [Complex Double]
test1Wave    = [1 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0]
-- expected: [1, 1, 1 ...]

test2Wave :: [Complex Double]
test2Wave    = [0 :+ 0, 1 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0]
{--
Expected Result: (from Numeric.FFT.fft in pure-fft package)
[1.0 :+ 0.0,
 0.7071067811865476 :+ (-0.7071067811865475),
 6.123233995736766e-17 :+ (-1.0),
 (-0.7071067811865475) :+ (-0.7071067811865476),
 (-1.0) :+ (-1.2246467991473532e-16),
 (-0.7071067811865477) :+ 0.7071067811865475,
 (-1.8369701987210297e-16) :+ 1.0,
 0.7071067811865475 :+ 0.7071067811865477]
 
 We're close, but our "nearly 0" values are a bit larger than Numeric.FFT's
 "nearly 0" values (e.g. 6.123233995736766e-17 vs 2.220446049250313e-16)
--}

benchConfig = defaultConfig {
                cfgSamples = ljust 1000
              }

main = do
  putStr "Square Wave Result:\n\t"
  putStrLn . show $ runFFT fft 16 squareWave
  putStr "Saw-Tooth Wave Result:\n\t"
  putStrLn . show $ runFFT fft 16 sawToothWave
  putStr "Test1 Wave Result:\n\t"
  putStrLn . show $ runFFT fft 8 test1Wave
  putStr "Test2 Wave Result:\n\t"
  putStrLn . show $ runFFT fft 8 test2Wave
  defaultMainWith benchConfig (return ()) [
        bgroup "fft" [ bench "square wave" $ nf (runFFT fft 16) squareWave,
                       bench "saw-tooth wave" $ nf (runFFT fft 16) sawToothWave,
                       bench "test1 wave" $ nf (runFFT fft 8) test1Wave,
                       bench "test2 wave" $ nf (runFFT fft 8) test2Wave
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
Test1 Wave Result:
	[1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0,
	 1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0]
Test2 Wave Result:
	[1.0 :+ 0.0,
	 0.7071067811865476 :+ (-0.7071067811865475),
	 2.220446049250313e-16 :+ (-1.0),
	 (-0.7071067811865474) :+ (-0.7071067811865477),
	 (-1.0) :+ 0.0,
	 (-0.7071067811865476) :+ 0.7071067811865475,
	 (-2.220446049250313e-16) :+ 1.0,
	 0.7071067811865474 :+ 0.7071067811865477]
warming up
estimating clock resolution...
mean is 8.010687 us (80001 iterations)
found 11361 outliers among 79999 samples (14.2%)
  5537 (6.9%) high mild
  5824 (7.3%) high severe
estimating cost of a clock call...
mean is 63.72107 ns (41 iterations)
found 1 outliers among 41 samples (2.4%)
  1 (2.4%) high mild

benchmarking fft/square wave
collecting 1000 samples, 252 iterations each, in estimated 8.032378 s
mean: 35.16367 us, lb 34.98511 us, ub 35.35924 us, ci 0.950
std dev: 3.011818 us, lb 2.788621 us, ub 3.320808 us, ci 0.950

benchmarking fft/saw-tooth wave
collecting 1000 samples, 215 iterations each, in estimated 8.027195 s
mean: 34.14124 us, lb 33.85232 us, ub 34.53818 us, ci 0.950
std dev: 5.412538 us, lb 4.306758 us, ub 7.160480 us, ci 0.950

benchmarking fft/test1 wave
collecting 1000 samples, 471 iterations each, in estimated 8.026252 s
mean: 15.32201 us, lb 15.25763 us, ub 15.42407 us, ci 0.950
std dev: 1.279731 us, lb 933.2923 ns, ub 2.250423 us, ci 0.950

benchmarking fft/test2 wave
collecting 1000 samples, 540 iterations each, in estimated 8.021356 s
mean: 15.95777 us, lb 15.88195 us, ub 16.04082 us, ci 0.950
std dev: 1.280561 us, lb 1.194957 us, ub 1.390523 us, ci 0.950
--}