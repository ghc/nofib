> module Main(main) -- floating point benchmark - Fourier transforms
> where             --     Rex Page (rpage@trc.amoco.com)
> import Fourier    --     Amoco Production Research, Sep 1992
> import Complex_Vectors
> import Complex--1.3

> main = putStr
>          ("result1 = " ++ show result1 ++ "\n" ++
>	    "result2 = " ++ show result2 ++ "\n" ++
>	    "result3 = " ++ show result3 ++ "\n")

> result1 =

>   --      tstfft(rmwC  256)
>         tstfft(rmwC  512)
>   --      tstfft(rmwC 1024)

> result2 =
>   --      tstdft(rmwC  256)
>         tstdft(rmwC  512)
>   --      tstdft(rmwC 1024)

> result3 =
>   --      tstsct(rampWave  256)
>         tstsct(rampWave  512)
>   --      tstsct(rampWave 1024)


  Test Apparatus

> tstfft zs  = distance zs (fftinv(fft zs))
> valfft zs  = distance (fft zs) (sft zs)

> tstdft zs  = distance zs (dftinv(dft zs))
> valdft zs  = distance (dft zs) (sft zs)

> tstsct = sum.sct

> squareWave n = take n (repeat 1)
> sqwC = (map (:+0)).squareWave

> rampWave n = [0 .. n-1]
> rmwC = (map (:+0)).rampWave

> sineWave = (map sin).thetas
> snwC = (map (:+0)).sineWave


  Haskell timings (in seconds)    hbc running on a SparcStation 1+
  rampWave
             n = 256         512          1024
    tstfft         8sec       17sec         41sec   22Sep92
    tstdft        14          36      out of heap   29Sep92
    tstsct        41         174           706      22Sep92
               8.4E6       67.E6         536.E6
  Note:  result should be approximately zero
         in all the above test cases, but isn't in sct cases
         (may indicate a problem with the hbc cosine routine;
          probably for large arguments -- such problems are
          unavoidable in general, but these results seem too
          far out, and they aren't consistent with Fortran
          and Miranda results)


