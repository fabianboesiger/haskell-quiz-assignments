-- Quiz Spring 2017
-- Assignment 3: Padding

import Control.Exception

main :: IO ()
main =
    assert (pad2 '*' "hello" == "h**e**l**l**o") $
    assert (pad2 '*' "++" == "+**+") $
    assert (pad2 '*' "x" == "x") $
    assert (pad2 '*' "" == "") $
    putStrLn "all tests passed"

pad2 :: Char -> String -> String
-- In case of an empty string, return an empty string.
pad2 c []     = []
-- If the string contains exactly one character, return the one character.
pad2 c [s]    = [s]
-- Solve the remaining cases recursively.
pad2 c (s:ss) = [s, c, c] ++ (pad2 c ss)
