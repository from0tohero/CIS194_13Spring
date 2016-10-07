import Data.List (sort)

-- EX 1
skips :: [a] -> [[a]]
skips s = [skipN i s | i <- [1 .. length s]]

skipN :: Int -> [a] -> [a]
skipN n s = [s!!(i*n) | i <- [1 .. length s]] 

-- EX 2
localMaxima :: [Integer] -> [Integer]
localMaxima (l1:rest@(h:l2:_))
    | l1 < h && h > l2 = h:(localMaxima rest)
    | otherwise        = localMaxima rest
localMaxima _ = []

-- EX 3
classify :: [Integer] -> Integer -> [(Integer, Integer)]
classify [] acc  =  [] -- only when the input is empty
classify [x] acc  = [(x,acc+1)]
classify (h:s:t) acc
    | h == s = classify (s:t) (acc+1)
    | otherwise = (h,acc+1):(classify (s:t) 0)

pad :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
pad 10 l = l
pad k [] = (k,0):(pad (k+1) [])
pad k l@(h:t)
    | k == fst h = h:(pad (k+1) t)
    | otherwise  = (k,0):(pad (k+1) l)

plot :: Integer -> [(Integer, Integer)] -> String
plot 0 l = "==========\n0123456789\n"
plot k l = [if (snd x) >= k then '*' else ' ' | x <- l] ++ "\n" ++ (plot (k-1) l)

histogram :: [Integer] -> String
histogram l = let c = classify (sort l) 0 in
              let s = pad 0 c in
              let max_ = maximum $ map snd s
              in plot max_ s