module Mersenne where 

import Lecture6Exm
import Composites
import Carmichael
import MillerRabin

import Control.Monad

get_mersenne k m = filterM (primeMR k) m_prime_list
	where
		m_prime_list = map (\p -> (2^p)-1) (take m primes)