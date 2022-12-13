-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

-- (a + b)^2 = a(a + b) + b(a + b)
--           = (a^2 + b^2) + 2(ab)

-- (a + b + c)^2 = a(a + b + c) + b(a + b + c) + c(a + b + c)
--               = (a^2 + b^2 + c^2) + 2(ab + 2ac + 2bc)

-- (a + b + c + d)^2 = a*(a + b + c + d) + b*(a + b + c + d) + c*(a + b + c + d) + d*(a + b + c + d)
--                   = (a^2 + b^2 + c^2 + d^2) + 2(ab + ac + ad + bc + bd + cd)

-- The difference is equal to twice the sum of every distinct pair of unequal numbers.
-- For some list [a .. z], that is
-- 2*(a*sum([b .. z]) + b*sum([c .. z]) + .. + y*sum([z]) + z*sum([]))

-- sum_of_unique_mixed_pairs [100000,99999..1]
-- 12500083332083325000
-- (558.76 secs, 446,091,700,104 bytes)
sum_of_unique_mixed_pairs [] = 0
sum_of_unique_mixed_pairs (x:xs) = (x * (sum xs)) + (sum_of_unique_mixed_pairs xs)

-- caches the first sum, but assumes list is in descending order, e.g. [10,9..1]
-- *Main> sum_of_unique_mixed_pairs' [100000,99999..1]
-- 12500083332083325000
-- (0.19 secs, 90,516,472 bytes)
sum_of_unique_mixed_pairs' [] = 0
sum_of_unique_mixed_pairs' (x:xs) = optimization x (sum xs)
optimization x y | x <= 1    = 0
                 | otherwise = (x * y) + optimization (x - 1) (y - (x - 1))


-- caches the first sum, and calculates the sum assuming it's a descending counting series
-- faster, but not correct, could be floating point issue
sum_of_unique_mixed_pairs'' [] = 0
sum_of_unique_mixed_pairs'' (x:xs) = optimization x (sum' xs)
sum' [] = 0
sum' [x] = x
sum' xs = ((x + y) * ((x - y) + 1)) / 2
    where x = head xs
          y = last xs

answer = 2 * (sum_of_unique_mixed_pairs [100,99..1])
