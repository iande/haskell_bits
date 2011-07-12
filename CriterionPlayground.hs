{-- Playing around with the criterion benchmark library --}
module Main where
  
import Criterion.Main
import Criterion.Config
import Data.List (unfoldr)

fibonacciNumbers :: [Int]
fibonacciNumbers = unfoldr (\(f1,f2) -> Just (f1, (f2, f1 + f2))) (0,1)

naiveFib :: Int -> Int
naiveFib 0 = 0
naiveFib 1 = 1
naiveFib n = naiveFib (n-2) + naiveFib (n-1)

accumFib :: Int -> Int
accumFib n = go n 0 1
           where go 0 f1 _  = f1
                 go 1 _ f2  = f2
                 go n f1 f2 = go (n - 1) f2 (f1 + f2)


-- Even only 100 samples is rather slow with naiveFib, let's lower this a bit
-- at the expense of timing quality.  I'm okay with that, as I'm just
-- observing the relative time difference between approaches.

benchConfig = defaultConfig {
               cfgSamples = ljust 50
             }

main = defaultMainWith benchConfig (return ()) [
       bgroup "naive fibs" [ bench "fib 10" $ nf naiveFib 10,
                             bench "fib 34" $ nf naiveFib 34,
                             bench "fib 37" $ nf naiveFib 37
                           ],
       bgroup "accum fibs" [ bench "fib 10" $ nf accumFib 10,
                             bench "fib 34" $ nf accumFib 34,
                             bench "fib 37" $ nf accumFib 37
                           ],
       bgroup "index fibs" [ bench "fib 10" $ nf (fibonacciNumbers !!) 10,
                             bench "fib 34" $ nf (fibonacciNumbers !!) 34,
                             bench "fib 37" $ nf (fibonacciNumbers !!) 37
                           ]
       ]
       
{--
warming up
estimating clock resolution...
mean is 7.520817 us (80001 iterations)
found 6757 outliers among 79999 samples (8.4%)
  4170 (5.2%) high mild
  2587 (3.2%) high severe
estimating cost of a clock call...
mean is 60.56562 ns (39 iterations)
found 3 outliers among 39 samples (7.7%)
  3 (7.7%) high mild

benchmarking naive fibs/fib 10
mean: 5.630743 us, lb 5.609372 us, ub 5.657412 us, ci 0.950
std dev: 86.19882 ns, lb 70.52051 ns, ub 118.2785 ns, ci 0.950
found 1 outliers among 50 samples (2.0%)
  1 (2.0%) high mild
variance introduced by outliers: 1.960%
variance is slightly inflated by outliers

benchmarking naive fibs/fib 34
collecting 50 samples, 1 iterations each, in estimated 29.53941 s
mean: 596.2728 ms, lb 593.0804 ms, ub 601.1695 ms, ci 0.950
std dev: 14.22331 ms, lb 9.848946 ms, ub 19.27489 ms, ci 0.950
found 5 outliers among 50 samples (10.0%)
  1 (2.0%) high mild
  4 (8.0%) high severe
variance introduced by outliers: 1.978%
variance is slightly inflated by outliers

benchmarking naive fibs/fib 37
collecting 50 samples, 1 iterations each, in estimated 135.4281 s
mean: 2.621285 s, lb 2.598780 s, ub 2.650476 s, ci 0.950
std dev: 92.60184 ms, lb 73.94102 ms, ub 117.4559 ms, ci 0.950
found 1 outliers among 50 samples (2.0%)
  1 (2.0%) high mild
variance introduced by outliers: 1.990%
variance is slightly inflated by outliers

benchmarking accum fibs/fib 10
mean: 2.145580 us, lb 2.130179 us, ub 2.171835 us, ci 0.950
std dev: 71.58153 ns, lb 47.40995 ns, ub 122.5643 ns, ci 0.950
found 4 outliers among 50 samples (8.0%)
  3 (6.0%) high mild
  1 (2.0%) high severe
variance introduced by outliers: 1.989%
variance is slightly inflated by outliers

benchmarking accum fibs/fib 34
mean: 7.134745 us, lb 7.089301 us, ub 7.201663 us, ci 0.950
std dev: 197.7428 ns, lb 146.3641 ns, ub 304.2780 ns, ci 0.950
found 1 outliers among 50 samples (2.0%)
  1 (2.0%) high mild
variance introduced by outliers: 1.984%
variance is slightly inflated by outliers

benchmarking accum fibs/fib 37
mean: 8.714965 us, lb 8.409929 us, ub 9.284972 us, ci 0.950
std dev: 1.474505 us, lb 919.1920 ns, ub 2.559461 us, ci 0.950
found 8 outliers among 50 samples (16.0%)
  5 (10.0%) high mild
  3 (6.0%) high severe
variance introduced by outliers: 2.000%
variance is slightly inflated by outliers

benchmarking index fibs/fib 10
mean: 74.35426 ns, lb 72.67969 ns, ub 76.22203 ns, ci 0.950
std dev: 6.429426 ns, lb 5.650283 ns, ub 7.210910 ns, ci 0.950
variance introduced by outliers: 1.998%
variance is slightly inflated by outliers

benchmarking index fibs/fib 34
mean: 193.8895 ns, lb 185.7618 ns, ub 211.1067 ns, ci 0.950
std dev: 41.31990 ns, lb 22.65642 ns, ub 72.06549 ns, ci 0.950
found 6 outliers among 50 samples (12.0%)
  3 (6.0%) high mild
  3 (6.0%) high severe
variance introduced by outliers: 2.000%
variance is slightly inflated by outliers

benchmarking index fibs/fib 37
mean: 171.9933 ns, lb 169.1350 ns, ub 175.6842 ns, ci 0.950
std dev: 11.74573 ns, lb 9.142649 ns, ub 16.14724 ns, ci 0.950
found 2 outliers among 50 samples (4.0%)
  2 (4.0%) high mild
variance introduced by outliers: 1.997%
variance is slightly inflated by outliers
--}