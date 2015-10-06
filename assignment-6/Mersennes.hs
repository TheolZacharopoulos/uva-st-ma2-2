module Mersennes where

import System.IO.Unsafe
import Lecture6

-- Using unsafeInterLeaveIO we can generate a lazy infinite list inside
-- the IO monad.
mersennes :: Int -> IO [Integer]
mersennes k = foldr f (return []) primes
    where f :: Integer -> IO [Integer] -> IO [Integer]
          f p ms = do
            b <- primeMR k mp
            unsafeInterleaveIO $
                if b then do
                        ms' <- ms
                        return (mp : ms')
                else ms
            where mp = 2^p - 1

main = do
    ms' <- mersennes 10
    print $ zipWith (==) ms ms'
    where ms = [m1,m2,m3,m4,m5,m6,m7,m8,m9,
                m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,
                m20,m21,m22,m23,m24,m25]
