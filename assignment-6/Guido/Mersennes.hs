module Mersennes where

import System.IO.Unsafe
import Lecture6

-- Using unsafeInterLeaveIO we can generate a lazy infinite list inside
-- the IO monad.
mersennes :: Int -> IO [Integer]
mersennes k = foldr f (return []) primes
    where f :: Integer -> IO [Integer] -> IO [Integer]
          f p ms = do
            b <- primeMR k p
            unsafeInterleaveIO $
                if b then do
                        ms' <- ms
                        return (mp : ms')
                else ms
            where mp = 2^p - 1
