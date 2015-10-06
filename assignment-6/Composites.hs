module Composites where 

import Lecture6

-- A composite number is any integer greater than one (>1)
-- that is not a prime number. (source: wikipedia)
composites :: [Integer]
composites = filter (not . isPrime) [2..]
