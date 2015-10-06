module Mersenne where 

import Lecture6Exm
import Composites
import Carmichael
import MillerRabin

import Control.Monad

-- Get the precision and the primes range.
get_mersenne k m = filterM (primeMR k) m_prime_list
	where
		m_prime_list = map (\p -> (2^p)-1) (take m primes)

mersenne n = do
	mers <- (get_mersenne 2 100)
	return $ take n mers