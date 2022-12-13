-- What is the 10001st prime number?

primes = [ n | n <- [2,3..10], all (>0) (map (mod n) primes) ]
