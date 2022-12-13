-- What is the largest prime factor of the number 600851475143?

-- Finds the prime factors of `n`. Starting with a `factor` of 2 (from the last case to the first):
--  1. If the given `factor` is not a factor of `n`, see if `factor + 1` is.
--  2. If the given `factor` is a factor of `n`, add `factor` to the list, and look for factors of
--     `n / factor`.
--  3. If the given `factor` is not a factor of `n` and `factor > sqrt(n)`, `n` must be prime - add
--     `n` to the list and return the list.
prime_factors n = prime_factors' n 2
    where
        prime_factors' n factor | n < factor*factor = [n] 
                                | mod n factor == 0 = factor : prime_factors' (div n factor) 2
                                | otherwise         = prime_factors' n (factor + 1)

answer = last (prime_factors 600851475143)

-- Interestingly, this version is not strictly faster than the above. It appears to use less memory,
-- (perhaps due to making half as many function calls?) but takes much longer to run (is it
-- expensive to read from an infinite list?). This comparison was made using `:set +s` before
-- running the functions in `ghci`.
worse_prime_factors n = prime_factors' n 0
    where
        factors = 2 : [3,5..]
        prime_factors' n i | n < factor*factor = [n] 
                           | mod n factor == 0 = factor : prime_factors' (div n factor) 0
                           | otherwise         = prime_factors' n (i + 1)
            where
                factor = factors !! i

-- also too slow
-- factors n candidates = [ i | i <- candidates, mod n i == 0 ]
-- prime_factors n = [ i | i <- factors_of_n, length(factors i factors_of_n) == 1 ] where factors_of_n = factors n [2 .. (n - 1)]
-- largest_prime_factor n = last (prime_factors n)

-- takes way, way too long
-- prime_factors n = [ i | i <- [2 .. (n - 1)], mod n i == 0, length(prime_factors i) == 0 ]
