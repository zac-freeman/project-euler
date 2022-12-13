-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any
-- remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

primes = sieve [2..]
   where sieve (prime:xs) = prime : sieve [x | x <- xs, mod x prime > 0]

-- get the largest power of each prime in [1..n]
largest_powers max = [ largest_power i 1 max | i <- takeWhile (<max) primes ]
    where largest_power prime exp max
            | prime^exp > max = prime^(exp - 1)
            | otherwise       = largest_power prime (exp + 1) max

-- The largest power of each prime contains as at least as many primes as any other number in the
-- range. Taking the product of all these powers ensures there are enough factors of each primes for
-- the product to be evenly divided by any number in the range.
answer = product (largest_powers 20)
