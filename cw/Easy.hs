module Easy where
import Data.List

solution :: String -> String -> Bool
solution = flip isSuffixOf --  drop (length s - length end) s == end

validBraces :: String -> Bool
validBraces xs = go xs "" where
    go :: String -> String -> Bool
    go str@(x : xs) stack@(y : ys) = case (x, y) of
        (')', '(') -> go xs ys
        (']', '[') -> go xs ys
        ('}', '{') -> go xs ys
        ('(', _) -> go xs (x : stack)
        ('[', _) -> go xs (x : stack)
        ('{', _) -> go xs (x : stack)
        (_, _) -> False
    go [] [] = True
    go ('(' : xs) [] = go xs ['(']
    go ('[' : xs) [] = go xs ['[']
    go ('{' : xs) [] = go xs ['{']
    go _ _ = False

validBraces' :: String -> Bool
validBraces' s = "" == foldr collapse [] s

collapse :: Char -> [Char] -> [Char]
collapse '(' (')':xs) = xs
collapse '{' ('}':xs) = xs
collapse '[' (']':xs) = xs
collapse x xs = x:xs