import Data.Char (toUpper)

--1
repeatN :: Int -> a -> [a]
repeatN 0 _ = []
repeatN 1 x = [x]
repeatN n x = [x] ++ repeatN (n-1) x

--2
longestList :: [[a]] -> [a]
longestList [[x]] = [x]
longestList [x, y] | (length x) > (length y) = x
                   | otherwise = y
longestList (x : (y : z)) = longestList (longestList [x, y] : z)

--3
intercalate :: [a] -> [[a]] -> [a]
intercalate s [] = []
intercalate s [x] = x
intercalate s (h:t) = h ++ s ++ intercalate s t

--4
onlyVowelsToUppercase :: String -> String
onlyVowelsToUppercase a =
    map toUpper (filterVowels a)

filterVowels :: String -> String
filterVowels a = filter (`elem` ['a','e','i','o','u','A','E','I','O','U']) a

--5
length2 :: [a] -> Int
length2 x = foldl f 0 x
    where f n m = n + 1

--6
applyList :: [a -> b] -> a -> [b]
applyList [f] n = [f n]
applyList (h:t) n = [h n] ++ applyList t n

--7
compose :: [a -> a] -> a -> a
compose [] v = v
compose (h:t) v = compose t (h v)
--is this possible to do using foldl instead?
--I tried, but couldn't get it to work :(
