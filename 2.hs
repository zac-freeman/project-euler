-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
fib = [1, 1] ++ [ fib !! (n - 1) + fib !! (n - 2) | n <- [2..1000] ]
answer = sum [ n | n <- fib, mod n 2 == 0, n < 4000000 ]
