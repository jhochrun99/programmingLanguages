w :: IO Int
w = do putStrLn "Enter your name: "
       name <- getLine
       putStrLn ("Nice to meet you, " ++ name)
       pure (length name)

--w :: IO Int
--w = putStrLn "Enter your name: " >>= \_ ->
--    getLine >>= \name ->
--    putStrLn ("Nice to meet you, " ++ name) >>= \_ ->
--    pure (length name)


--1

firstWords :: FilePath -> IO ()
firstWords f  = do file <- readFile f
                   fileLines <- pure (lines file)
                   fileWords <- pure (map words fileLines)
                   mapM_ putFirst fileWords

putFirst :: [String] -> IO ()
putFirst [] = putStrLn ""
putFirst (h:t) = putStrLn h

--2
firstWords2 :: FilePath -> IO ()
firstWords2 f = (readFile f) >>=
              (\file -> pure  (lines file)) >>=
              (\fileLines -> pure (map words fileLines)) >>=
              (\fileWords ->  mapM_ putFirst fileWords)

--3
mymapM :: Monad m => (a -> m b) -> [a] -> m [b]
mymapM f [] = pure []
mymapM f (h:t) = (f h) >>=
                 (\m -> (mymapM f t) >>=
                 (\v -> pure (m : v)))

--4
whileTrue :: IO Bool -> IO ()
whileTrue f = f >>=
              (\val -> case val of
                True -> whileTrue f
                _ -> pure () )

--5
q :: [Int]
q = [1,2,3] >>=
    (\a -> [100,200,300] >>=
    (\b -> pure (a + b)))

--6
{-
 - Originally I thought it'd be [101, 202, 303]
 - but when I looked it over more carefully, I realized it had to be more of
 - a nested loop type scenario. Each item in a would be added to each item in
 - b, resulting in [101, 102, 103, 201, 202, 203, 301, 302, 303]
 - something like "for i in a: for j in b: add a+b"
 -}

--7
diceSumTo :: Int -> [[Int]]
diceSumTo n =
  do firstDie <- [1 .. 6]
     secondDie <- [1 .. 6]
     thirdDie <- [1 .. 6]
     result <- case (firstDie + secondDie + thirdDie) /= n of
         True -> pure []
         False -> pure [firstDie, secondDie, thirdDie]
     removeEmptyLists [result]

removeEmptyLists :: [[Int]] -> [[Int]]
removeEmptyLists [] = []
removeEmptyLists ([]:t) = removeEmptyLists t
removeEmptyLists (h:t) = h : removeEmptyLists t
