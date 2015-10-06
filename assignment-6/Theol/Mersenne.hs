module Mersenne where 

import Lecture6Exm
import Composites
import Carmichael
import MillerRabin

import Control.Monad

-- Get the precision and the wanted mersenne number.
get_mersenne k m = filterM (primeMR k) m_prime_list
	where
		m_prime_list = map (\p -> (2^p)-1) (take (m*10) primes) -- Todo: Make it unsafePerformIO ???

mersenne n = do
	mers <- (get_mersenne 2 n)
	return $ take n mers