module Main where

import FFT
import Criterion.Config
import Criterion.Main
import Data.Complex

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
  putStrLn . show $ fft squareWave
  putStr "Saw-Tooth Wave Result:\n\t"
  putStrLn . show $ fft sawToothWave
  putStr "Test1 Wave Result:\n\t"
  putStrLn . show $ fft test1Wave
  putStr "Test2 Wave Result:\n\t"
  putStrLn . show $ fft test2Wave
  defaultMainWith benchConfig (return ()) [
        bgroup "fft" [ bench "square wave" $ nf fft squareWave,
                       bench "saw-tooth wave" $ nf fft sawToothWave,
                       bench "test1 wave" $ nf fft test1Wave,
                       bench "test2 wave" $ nf fft test2Wave
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
mean is 7.065876 us (80001 iterations)
found 3543 outliers among 79999 samples (4.4%)
 3002 (3.8%) high severe
estimating cost of a clock call...
mean is 60.72323 ns (70 iterations)
found 4 outliers among 70 samples (5.7%)
 2 (2.9%) high mild
 2 (2.9%) high severe

benchmarking fft/square wave
collecting 1000 samples, 322 iterations each, in estimated 7.079556 s
mean: 21.91537 us, lb 21.84829 us, ub 22.00604 us, ci 0.950
std dev: 1.248337 us, lb 1.011847 us, ub 1.578042 us, ci 0.950

benchmarking fft/saw-tooth wave
collecting 1000 samples, 315 iterations each, in estimated 7.087444 s
mean: 21.98863 us, lb 21.93955 us, ub 22.05070 us, ci 0.950
std dev: 888.0943 ns, lb 754.7825 ns, ub 1.121602 us, ci 0.950

benchmarking fft/test1 wave
collecting 1000 samples, 811 iterations each, in estimated 7.068688 s
mean: 8.627667 us, lb 8.609107 us, ub 8.651401 us, ci 0.950
std dev: 337.2305 ns, lb 278.9374 ns, ub 431.2060 ns, ci 0.950

benchmarking fft/test2 wave
collecting 1000 samples, 821 iterations each, in estimated 7.068300 s
mean: 8.712524 us, lb 8.685743 us, ub 8.743774 us, ci 0.950
std dev: 466.3676 ns, lb 414.3701 ns, ub 542.8018 ns, ci 0.950
--}
