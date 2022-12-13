-- A palindromic number reads the same both ways. The largest palindrome made from the product of
-- two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

order_of n = floor (logBase 10 (fromIntegral n))

digit_at n i = mod (div n (10^i)) 10

flip_num n = sum [ (digit_at n i) * (10^((order_of n) - i)) | i <- [0 .. (order_of n)] ]

-- This is iterating over 899^2 products to find all of the palindromic products of 2 3-digit
-- numbers. This took almost 12 GB of memory. Is there some property of palindromes that would allow
-- us to skip some of these? Changing the range from [999,100] to [999,800] cuts the memory usage to
-- about 600 MB.
palindromes = [ n | i <- [999,998..100], j <- [999,998..100], let n = i * j, n == flip_num n]

max_of []  = 0
max_of [x] = x
max_of (x:xs)
  | x > y     = x
  | otherwise = y
    where y = max_of xs

answer = max_of palindromes
