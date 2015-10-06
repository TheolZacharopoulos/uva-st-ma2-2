module Mersenne where 

import Lecture6Exm
import Composites
import Carmichael
import MillerRabin

import Control.Monad

get_mersenne k m = filterM (primeMR k) m_prime_list
	where
		prime_list = (take m primes)
		m_prime_list = map (\n -> (2^n)-1) prime_list