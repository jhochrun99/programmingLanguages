--2
data Tree a = Node a (Tree a) (Tree a)
            | Leaf
            deriving Show

{- Type of Leaf is Tree a
 - Type of Leaf :; Tree Int is Tree Int
 - Type of Node "hello" is Tree [Char] -> Tree [Char] -> Tree [Char]
 - Type of Node True Leaf is Tree [Bool] -> Tree [Bool]
 -}

someTree :: Tree String
someTree = Node "t1"
            (Node "t2a"
                (Node "t3a" Leaf Leaf)
                (Node "t3b" Leaf Leaf))
            (Node "t2b"
                (Node "t3c" Leaf Leaf)
                (Node "t3d" Leaf Leaf))

flipNode :: Tree a -> Tree a
flipNode (Node value left right) = Node value right left
flipNode Leaf = Leaf

{- Type of flipNode Leaf is Tree a
 - Type of flipNode (Node False Leaf Leaf) is Tree Bool
 -}

--3
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node value l r)  = Node (f value) (mapTree f l) (mapTree f r)
mapTree f Leaf = Leaf

--4
flipTree :: Tree a -> Tree a
flipTree (Node v l r) = Node v (flipTree r) (flipTree l)
flipTree Leaf = Leaf
{-
 - flipTree is very similar to flipNode, except instead of
 - regularly returning the left and right child like flipNode,
 - it goes through and flips left and right recursively as well.
 -}

--5
--inorder: left, value, right
inorderTraversal :: Tree a -> [a]
inorderTraversal Leaf = []
inorderTraversal (Node v Leaf Leaf) = [v]
inorderTraversal (Node v l r) =
    (inorderTraversal l) ++ [v] ++ (inorderTraversal r)

--6
getLevel :: Int -> Tree a -> [a]
getLevel 0 (Node v l r) = [v]
getLevel n Leaf  = []
getLevel n (Node v l r) =
    (getLevel (n-1) l) ++ (getLevel (n-1) r)
-- getLevel 9 is still Tree a -> [a]
-- if using someTree, it'd be []

--7
mylast :: [a] -> Maybe a
mylast [] = Nothing
mylast [x] = Just x
mylast (h:t) = mylast t
