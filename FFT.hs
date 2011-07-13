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
    fft
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

fft :: [Complex Double] -> [Complex Double]
fft []      = []
fft [x1,x2] = [ (x1 + x2), (x1 - x2) ]
fft xs      = let n       = fromIntegral (length xs)
                  wn      = conjugate $ cis (2.0 * pi / n)
                  m       = round (n / 2)
                  (x1,x2) = butterflyList xs
                  y1      = fft x1
                  y2      = fft x2
                  w       = 1 :+ 0
                  (_,(c1s,c2s)) = foldl cRes (w, ([],[])) (zip y1 y2)
                  cRes (k, (ps,ms)) (v1,v2) = let kv2 = k*v2
                                                  vP  = v1 + kv2
                                                  vM  = v1 - kv2
                                                  in (k*wn, (vP:ps, vM:ms))
                  in (reverse c1s ++ reverse c2s)
{--
fft [0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0,
     0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0, 1 :+ 0]

=>  [8 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0,
    -8 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0]
--}
