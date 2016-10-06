import Data.List (sort)

-- EX 1
skipN :: [a] -> Int -> [a] -> Int -> [a]
skipN [] _ aux _ = reverse aux
skipN (h:t) 1 aux k = skipN t k (h:aux) k
skipN (h:t) m aux k = skipN t (m-1) aux k

countDown :: [a] -> Int -> [[a]] -> [[a]]
countDown s 0 aux = aux
countDown s k aux = countDown s (k-1) ((skipN s k [] k):aux)

skips :: [a] -> [[a]]
skips s = countDown s (length s) []

-- EX 2
localMaxima1 :: [Integer] -> [Integer] -> [Integer]
localMaxima1 (l1:h:l2:rest) aux
    | l1 < h && h > l2 = localMaxima1 (h:l2:rest) (h:aux)
    | otherwise        = localMaxima1 (h:l2:rest) aux
localMaxima1 _ aux = reverse aux

localMaxima :: [Integer] -> [Integer]
localMaxima s = localMaxima1 s []

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