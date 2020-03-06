-- Quiz Spring 2017
-- Assignment 2: Alternating Numbers

import Control.Exception

main :: IO ()
main =
    assert (evenAlt 81634) $
    assert (not $ evenAlt 2345) $
    assert (not $ evenAlt 9134) $
    assert (evenAlt 0) $
    assert (not $ evenAlt 1) $
    assert (evenAlt 12) $
    putStrLn "all tests passed"

evenAlt :: Int -> Bool
evenAlt x
    | x < 0     = error "negative argument"
    -- We set b to True as the least significant digit should be even.
    | otherwise = go x True
    where
        go :: Int -> Bool -> Bool
        -- x is the remaining number, b is True when the next numer should be even, False otherwise.
        go x b
            | x < 10    = even x == b
            -- We recurively call go with (not b) as we want the digits to alternate between even and odd.
            | otherwise = even x == b && go (x `div` 10) (not b) 