Some odds and ends of Haskell code as I write it and have no better place to
stuff it.

## FFT.hs

An implementation of the
[Cooley-Tukey FFT algorithm](http://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm)
derived from Justin Grant's
[Qi implementation](http://code.google.com/p/jngmisc/source/browse/qi/fft.qi).

Benchmarks for this implementation can be found in [FFTBench.hs](FFTBench.hs)
demonstrating its performance against Matt Morrow's
[pure-fft](http://hackage.haskell.org/package/pure-fft) package for various
inputs.  It's important to note that I doubt there is any significance to the
difference in running times as both implementations are pretty similar.  The
primary difference is my use of `cis` instead of `exp`, which produces slightly
different outputs (particularly when the values are "close" to zero.)

