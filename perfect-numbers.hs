-- Mock Quiz Spring 2020
-- Assignment 2: Perfect Numbers

import Control.Exception

main :: IO ()
main =
    assert (not $ perfect 1) $
    assert (perfect 6) $
    assert (not $ perfect 7) $
    assert (perfect 28) $
    assert (not $ perfect 1000) $
    assert (perfect 8128)
    putStrLn "all tests passed"


perfect :: Int -> Bool
-- Check if the sum of all divisors of n is equal to n.
perfect n = sum divisors == n
    where
        -- The divisors list contains every divisor of n except itself.
        divisors = [d | d <- [1..(n - 1)], n `mod` d == 0]
    