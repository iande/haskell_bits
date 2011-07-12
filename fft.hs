{--

Derived from Justin Grant's FFT implementation in Qi.

  http://code.google.com/p/jngmisc/source/browse/qi/fft.qi
  
As a derivative work, the license for this implementation will remain the
same as Grant's.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

 Redistributions of source code must retain the above copyright notice, this list 
 of conditions and the following disclaimer.
 Redistributions in binary form must reproduce the above copyright notice, this 
 list of conditions and the following disclaimer in the documentation and/or 
 other materials provided with the distribution.
 Neither the name of the <ORGANIZATION> nor the names of its contributors may be 
 used to endorse or promote products derived from this software without specific 
 prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
 NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
 EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--}

module FFT
  (
    fft,
    runFft
  ) where

import Data.Complex
import Data.List (unfoldr)
import System.CPUTime

-- Note: this function will fail if the list contains an odd number of elements.
butterflyPair :: [a] -> Maybe ((a,a), [a])
butterflyPair [] = Nothing
butterflyPair (x1:x2:xs) = Just ((x1, x2), xs)

butterflyList :: [a] -> ([a], [a])
butterflyList = unzip . (unfoldr butterflyPair)

{--
butterflyList [1 :+ 0, 2 :+ 0, 3 :+ 0, 4 :+ 0, 5 :+ 0, 6 :+ 0, 7 :+ 0, 8 :+ 0,
               0 :+ 1, 0 :+ 2, 0 :+ 3, 0 :+ 4, 0 :+ 5, 0 :+ 6, 0 :+ 7, 0 :+ 8]
          => ([1 :+ 0, 3 :+ 0, 5 :+ 0, 7 :+ 0, 0 :+ 1, 0 :+ 3, 0 :+ 5, 0 :+ 7],
              [2 :+ 0, 4 :+ 0, 6 :+ 0, 8 :+ 0, 0 :+ 2, 0 :+ 4, 0 :+ 6, 0 :+ 8])
--}

calcResults :: (RealFloat a) => (([Complex a], [[Complex a]]), ([Complex a], [Complex a])) -> (([Complex a], [[Complex a]]), ([Complex a], [Complex a]))
calcResults (([w, wn], [ya, yb]), ([], [])) = (([w, wn], [reverse ya, reverse yb]), ([], []))
calcResults (([w, wn], [ya, yb]), (y1:y1s, y2:y2s)) = calcResults (([ w*wn, wn], [(y1 + w*y2) : ya, (y1 - w*y2) : yb]), (y1s, y2s))


fft :: (RealFloat a) => Integer -> Complex a -> [Complex a] -> [Complex a] -> [Complex a]
fft 1 wn (x:xs) _ = [x]
fft 2 wn (x1:x2:xs) _ = [ (x1 + x2), (x1 - x2)]
fft n wn xs ys = let m       = round (fromIntegral(n) / 2)
                     (x1,x2) = butterflyList xs
                     wn'     = wn * wn
                     y1      = fft m wn' x1 []
                     y2      = fft m wn' x2 []
                     w       = 1 :+ 0
                     res     = calcResults (([w,wn], [[], []]), (y1, y2))
                     in (head (snd (fst res))) ++ (head (tail (snd (fst res))))
                     
{--
(fft 16 [0.923880 -0.382683]
     [[0 0] [1 0] [0 0] [1 0] [0 0] [1 0] [0 0] [1 0] 
      [0 0] [1 0] [0 0] [1 0] [0 0] [1 0] [0 0] [1 0]] [])

fft 16 (0.923880 :+ (-0.382683))
    [0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0,
     0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0] []

=>  [8 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0,
    -8 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0]
--}

doTimesFft :: (RealFloat a) => Integer -> Integer -> Complex a -> [Complex a] -> [Complex a] -> [Complex a]
doTimesFft 0 _ _ _ res         = res
doTimesFft iters sz w inps res = doTimesFft (iters - 1) sz w inps (fft sz w inps [])

runFft :: (RealFloat a) => Integer -> Integer -> [Complex a] -> [Complex a]
runFft iters sz inps = let theta = (2.0 * pi / fromIntegral(sz))
                           w = conjugate $ cis theta
                           in doTimesFft iters sz w inps []
                           
                           
-- Timing the function
timeM :: (Show b) => (a -> IO b) -> a -> IO String
timeM action arg =
  do startTime <- getCPUTime
     res <- action arg
     finishTime <- getCPUTime
     putStrLn (show res)
     return $ show (fromIntegral (finishTime - startTime) / 1000000000000)
 
-- Version for use with evaluating regular non-monadic functions
time :: (Show b) => (a -> b) -> a -> IO String
time f = timeM (\x -> let y = f x in y `seq` return (show y))

{--
Square wave test
time (runFft 100000 16)
  [0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0,
   0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0]

"[8.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,
  0.0 :+ 0.0,0.0 :+ 0.0,(-8.0) :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,
  0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0]"
"0.103592"

Saw-tooth wave

time (runFft 10000 16)
  [1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0, 1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0,
   1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0, 1 :+ 0, 2 :+ 0, 3 :+ 0, 0 :+ 0]
   
"[24.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,(-8.0) :+ (-8.0),0.0 :+ 0.0,
   0.0 :+ 0.0,0.0 :+ 0.0,8.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,
   (-7.999999999999999) :+ 8.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0]"
"1.0655e-2"
--}