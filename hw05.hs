--1
{-
 - a) pass by value
 -    6 [1,2,3]   #res mylist
 - b) pass by reference
 -    6 [0,0,0]   #res mylist
 -}

--2
{-
 - It isn't tail recursive, because you still need to do
 - addition on the last line, after the recursion is complete
 -
 - Can't be made tail recursive
 - The function is adding elements of a list, and a list
 - Unless the elements of the list are also lists (list of lists)
 - then it won't work at all.
 - You can do [1,2,3] + [0,0,0] but not 1 + [0]
 -
 - You /can/ make a tail recursive function for adding elements
 - in a list of lists if you alter the form of the function itself
 - (and, again, assume lst must be a list of lists, like [[1]])
 -
 - def mystery(lst, combined=[]):
 -    if len(lst) == 0:
 -      return combined
 -    return mystery(lst[1:], combined+lst[0])
 -}

--3
data Stream a = Chunk a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Chunk v next) = v : streamToList next

streamRepeat :: a -> Stream a
streamRepeat a = Chunk a (streamRepeat a)

--4
instance Functor Stream where
  fmap f (Chunk v next) = Chunk (f v) (fmap f next)

--5
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Chunk a (streamFromSeed f (f a))

--6
data Tree a = Node a (Tree a) (Tree a)
            | Leaf
            deriving Show

bigTree :: Tree Integer
bigTree = bigBuilder 0

bigBuilder :: Integer -> Tree Integer
bigBuilder n = Node n (bigBuilder (n+1)) (bigBuilder (n+1))
