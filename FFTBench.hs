module FFTBench where

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
  putStrLn $ "Square Wave Result: " ++ (show $ runFFT 16 squareWave)
  putStrLn $ "Saw-Tooth Wave Result: " ++ (show $ runFFT 16 sawToothWave)
  defaultMainWith benchConfig (return ()) [
        bgroup "fft" [ bench "square wave" $ nf (runFFT 16) squareWave,
                       bench "saw-tooth wave" $ nf (runFFT 16) sawToothWave
                     ]
       ]

{--
Square Wave Result: [8.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,(-8.0) :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0]
Saw-Tooth Wave Result: [24.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,(-8.0) :+ (-8.0),0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,8.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,(-7.999999999999999) :+ 8.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0]
warming up
estimating clock resolution...
mean is 7.824400 us (80001 iterations)
found 3390 outliers among 79999 samples (4.2%)
  2989 (3.7%) high severe
estimating cost of a clock call...
mean is 63.56386 ns (76 iterations)
found 2 outliers among 76 samples (2.6%)
  2 (2.6%) high mild

benchmarking fft/square wave
collecting 1000 samples, 45 iterations each, in estimated 7.936992 s
mean: 179.4975 us, lb 179.0982 us, ub 180.0552 us, ci 0.950
std dev: 7.503796 us, lb 5.733769 us, ub 9.836049 us, ci 0.950

benchmarking fft/saw-tooth wave
collecting 1000 samples, 44 iterations each, in estimated 7.920593 s
mean: 180.3684 us, lb 179.8300 us, ub 181.1229 us, ci 0.950
std dev: 10.17635 us, lb 7.919220 us, ub 13.15644 us, ci 0.950
--}