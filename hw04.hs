data Tree a = Node a (Tree a) (Tree a)
            | Leaf

someTree :: Tree String
someTree = Node "t1"
            (Node "t2a"
                (Node "t3a" Leaf Leaf)
                (Node "t3b" Leaf Leaf))
            (Node "t2b"
                (Node "t3c" Leaf Leaf)
                (Node "t3d" Leaf Leaf))

--1
instance Eq a => Eq (Tree a) where
    Leaf == Leaf = True
    (Node val left right) == Leaf = False
    Leaf == (Node val left right) = False
    (Node val1 left1 right1) == (Node val2 left2 right2) =
        val1 == val2 && left1 == left2 && right1 == right2

--2
instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)

--3
bstInsert :: Ord a => Tree (a, Int) -> a -> Tree (a, Int)
bstInsert Leaf v = Node (v, 1) Leaf Leaf
bstInsert (Node (a, n) l r) v
    | a == v = Node (a, n+1) l r
    | a < v = Node (a,n) l (bstInsert r v)
    | a > v = Node (a,n) (bstInsert l v) r

--4
bstLookup :: Ord a => Tree (a, Int) -> a -> Int
bstLookup Leaf v = 0
bstLookup (Node (a, n) l r) v
    | a == v = n
    | a < v = bstLookup r v
    | a > v = bstLookup l v

--5
bstRemove :: Ord a => Tree (a, Int) -> a -> Maybe (Tree (a, Int))
bstRemove Leaf v = Nothing
bstRemove (Node (a, n) l r) v
    | a == v && n > 1 = Just (Node (a, n-1) l r)
    | a == v = case r of
        Leaf -> Just (l)
        _ -> case bstReplace r of
            Node v Leaf r' -> Just (Node v l r')
    | a > v = case bstRemove l v of
        Nothing -> Nothing
        Just (t) -> Just (Node (a,n) t r)
    | a < v = case bstRemove r v of
        Nothing -> Nothing
        Just (t) -> Just (Node (a,n) l t)

--finds the leftmost child and replaces the Node being removed
bstReplace :: Ord a => Tree (a, Int) -> Tree (a, Int)
bstReplace Leaf = Leaf
bstReplace (Node a l r) = case bstReplace l of
    Leaf -> (Node a Leaf r)
    (Node a' l' r') -> (Node a' l' (Node a Leaf r))

--6
instance Show a => Show (Tree a) where
    show t = printout 0 t

printout :: Show a => Int -> Tree a -> String
printout 0 Leaf = "Leaf"
printout _ Leaf = ""
printout n (Node a l r)
    | n == 0 = "Node " ++ (show a) ++ "\n" ++ (printout (n+1) l) ++ (printout (n+1) r)
    | otherwise =
        (indent n "  " "  ") ++ "Node " ++ (show a) ++ "\n" ++
            (printout (n+1) l) ++ (printout (n+1) r)
{-
 - if run on mybst
 -   let mybst = foldl bstInsert Leaf [5,12,9,2,3,-4,19,25,9,2]
 - it outputs the values in format Node (a,b)
 -}

--would i be allowed to use 'replicate' in hw? like replicate 4 "    "
--instead of writing a helper function for it?
indent :: Int -> [Char] ->  String -> String
indent 0 _  _ = ""
indent 1 c s = s
indent n c s = indent (n-1) c (s ++ c)
