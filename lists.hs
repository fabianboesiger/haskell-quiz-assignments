-- Quiz Spring 2011
-- Assignment 3: Lists

import Control.Exception

main :: IO ()
main =
    assert (Main.maximum [1, 2, 3, 1, 3] == 3) $
    assert (Main.maximum [-3.0 , 4.2, 342.3, 23.4] == 342.3) $
    assert ((concatMap (\a -> [a, 2 * a]) [1, 2, 3]) == (concatMap' (\a -> [a, 2 * a]) [1, 2, 3]))
    assert (take 10 (iter2 ((+) 1) ((*) 2) 1 1) == [1, 1, 2, 2, 3, 4, 4, 8, 5, 16]) $
    putStrLn "all tests passed"

maximum :: Ord a => [a] -> a
maximum []     = error "expected non-empty list"
maximum [x]    = x
maximum (x:xs) = if x > y then x else y 
    where
        y = Main.maximum xs

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = foldr aux e
    where
        aux x acc = f x ++ acc
        e         = []

iter2 :: (a -> a) -> (a -> a) -> a -> a -> [a]
iter2 f g x y = [x, y] ++ (iter2 f g(f x) (g y))