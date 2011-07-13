module Main where

import FFT
import Criterion.Config
import Criterion.Main
import Data.Complex
import qualified Numeric.FFT as NF

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
                cfgSamples = ljust 500
              }

main = do
  putStr "Square Wave Result:\n\t"
  putStrLn . show $ fft squareWave
  putStr "\t"
  putStrLn . show $ NF.fft squareWave
  putStr "Saw-Tooth Wave Result:\n\t"
  putStrLn . show $ fft sawToothWave
  putStr "\t"
  putStrLn . show $ NF.fft sawToothWave
  putStr "Test1 Wave Result:\n\t"
  putStrLn . show $ fft test1Wave
  putStr "\t"
  putStrLn . show $ NF.fft test1Wave
  putStr "Test2 Wave Result:\n\t"
  putStrLn . show $ fft test2Wave
  putStr "\t"
  putStrLn . show $ NF.fft test2Wave
  defaultMainWith benchConfig (return ()) [
        bgroup "fft" [ bench "square wave" $ nf fft squareWave,
                       bench "saw-tooth wave" $ nf fft sawToothWave,
                       bench "test1 wave" $ nf fft test1Wave,
                       bench "test2 wave" $ nf fft test2Wave
                     ],
        bgroup "FFT" [ bench "square wave" $ nf NF.fft squareWave,
                       bench "saw-tooth wave" $ nf NF.fft sawToothWave,
                       bench "test1 wave" $ nf NF.fft test1Wave,
                       bench "test2 wave" $ nf NF.fft test2Wave
                     ]
       ]

{--
Results shown below come from compiling and running the benchmarks:

      ghc -o fftbench FFTBench.hs
      ./fftbench
--}

{--
Square Wave Result:
	[8.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,(-8.0) :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0]
	[8.0 :+ 0.0,(-3.2001563056406604e-16) :+ (-2.465190328815662e-32),(-3.463824224941973e-16) :+ (-4.930380657631324e-32),(-1.3255481435101412e-16) :+ (-4.3140830754274083e-32),(-4.898587196589413e-16) :+ (-2.999519565323715e-32),(-1.3255481435101407e-16) :+ (-1.232595164407831e-32),(-3.4638242249419727e-16) :+ (-7.395570986446986e-32),(-3.20015630564066e-16) :+ (-7.395570986446986e-32),(-8.0) :+ (-9.797174393178826e-16),3.20015630564066e-16 :+ 4.930380657631324e-32,3.463824224941973e-16 :+ 7.395570986446986e-32,1.3255481435101412e-16 :+ 1.1709654061874394e-31,4.898587196589413e-16 :+ 8.998558695971146e-32,1.325548143510141e-16 :+ (-2.465190328815662e-32),3.463824224941973e-16 :+ 9.860761315262648e-32,3.20015630564066e-16 :+ 2.711709361697228e-31]
Saw-Tooth Wave Result:
	[24.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,(-8.0) :+ (-8.0),0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,8.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,(-7.999999999999999) :+ 8.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0]
	[24.0 :+ 0.0,(-9.620539442200974e-16) :+ (-2.550194942657494e-16),(-1.081170501982609e-15) :+ (-5.913117823236681e-16),(-5.296637681775746e-16) :+ (-4.4248031047880143e-16),(-7.999999999999999) :+ (-8.000000000000002),2.6455413947554646e-16 :+ 1.9755095064933072e-16,3.884056569942145e-16 :+ 1.0145306266472677e-16,3.2202268309196523e-16 :+ 1.0090134436278821e-17,8.0 :+ (-9.797174393178826e-16),(-3.220226830919653e-16) :+ 1.0090134436278747e-17,(-3.884056569942146e-16) :+ 1.0145306266472672e-16,(-2.645541394755467e-16) :+ 1.975509506493307e-16,(-8.000000000000002) :+ 7.999999999999998,5.296637681775746e-16 :+ (-4.424803104788014e-16),1.0811705019826093e-15 :+ (-5.913117823236677e-16),9.620539442200974e-16 :+ (-2.55019494265749e-16)]
Test1 Wave Result:
	[1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0]
	[1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0]
Test2 Wave Result:
	[1.0 :+ 0.0,0.7071067811865476 :+ (-0.7071067811865475),2.220446049250313e-16 :+ (-1.0),(-0.7071067811865474) :+ (-0.7071067811865477),(-1.0) :+ 0.0,(-0.7071067811865476) :+ 0.7071067811865475,(-2.220446049250313e-16) :+ 1.0,0.7071067811865474 :+ 0.7071067811865477]
	[1.0 :+ 0.0,0.7071067811865476 :+ (-0.7071067811865475),6.123233995736766e-17 :+ (-1.0),(-0.7071067811865475) :+ (-0.7071067811865476),(-1.0) :+ (-1.2246467991473532e-16),(-0.7071067811865477) :+ 0.7071067811865475,(-1.8369701987210297e-16) :+ 1.0,0.7071067811865475 :+ 0.7071067811865477]
warming up
estimating clock resolution...
mean is 7.177350 us (80001 iterations)
found 4280 outliers among 79999 samples (5.4%)
  945 (1.2%) high mild
  3335 (4.2%) high severe
estimating cost of a clock call...
mean is 59.99256 ns (71 iterations)
found 4 outliers among 71 samples (5.6%)
  3 (4.2%) high mild
  1 (1.4%) high severe

benchmarking fft/square wave
mean: 21.40527 us, lb 21.34238 us, ub 21.49538 us, ci 0.950
std dev: 848.4830 ns, lb 650.0032 ns, ub 1.196819 us, ci 0.950

benchmarking fft/saw-tooth wave
mean: 21.51137 us, lb 21.45394 us, ub 21.59091 us, ci 0.950
std dev: 764.8819 ns, lb 608.0175 ns, ub 1.181096 us, ci 0.950

benchmarking fft/test1 wave
mean: 8.490940 us, lb 8.433948 us, ub 8.560772 us, ci 0.950
std dev: 718.7744 ns, lb 617.2981 ns, ub 839.3353 ns, ci 0.950

benchmarking fft/test2 wave
mean: 8.265013 us, lb 8.244972 us, ub 8.296082 us, ci 0.950
std dev: 279.6293 ns, lb 208.7070 ns, ub 446.7732 ns, ci 0.950

benchmarking FFT/square wave
mean: 22.35740 us, lb 22.26577 us, ub 22.48706 us, ci 0.950
std dev: 1.238925 us, lb 959.7430 ns, ub 1.667010 us, ci 0.950

benchmarking FFT/saw-tooth wave
mean: 22.07682 us, lb 22.03094 us, ub 22.15129 us, ci 0.950
std dev: 651.4264 ns, lb 439.2713 ns, ub 1.020790 us, ci 0.950

benchmarking FFT/test1 wave
mean: 9.033085 us, lb 9.011123 us, ub 9.070515 us, ci 0.950
std dev: 319.6867 ns, lb 219.4151 ns, ub 485.2801 ns, ci 0.950

benchmarking FFT/test2 wave
mean: 9.071208 us, lb 9.047375 us, ub 9.107935 us, ci 0.950
std dev: 332.3800 ns, lb 244.7853 ns, ub 507.8146 ns, ci 0.950
--}
