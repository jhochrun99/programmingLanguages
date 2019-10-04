--1
discriminant :: Double -> Double -> Double -> Double
discriminant a b c = (b*b)-4*a*c

--2
harmonic :: Int -> Double
harmonic 0 = 0
harmonic n = (1/ fromIntegral n) + harmonic (n-1)

--3
countQs :: String -> Int
countQs "" = 0
countQs (h:t) =
  if(h == 'q' || h == 'Q') then
    1 + countQs t
  else
    countQs t

--4
mytake :: Int -> [String] -> [String]
mytake n [] = []
mytake 0 l = []
mytake n (h:t) = h : mytake (n-1) t

--5
mylast :: [Int] -> Int
mylast [] = 0
mylast [x] = x
mylast (h:t) = mylast t

--6
range :: Int -> Int -> [Int]
range x y =
  if x > y then
    []
  else if x == y then
    [x]
  else
    [x] ++ range (x+1) y
