module Main where
import Data.Char
import Data.List

type Vec = (Int, Int)

parseLine :: Char -> String -> [Int]
parseLine sep inp = aux inp Nothing []
  where 
    aux :: String -> Maybe Int -> [Int] -> [Int]
    aux (c:cs) (Just d) parsed
      | c == sep  = aux cs  Nothing  (d:parsed)
      | otherwise = aux cs (Just d')  parsed
      where d' = d * 10 + (digitToInt c :: Int)

    aux (c:cs) Nothing parsed
      | c == sep  = aux cs  Nothing   parsed
      | otherwise = aux cs (Just d')  parsed
      where d' = digitToInt c :: Int

    aux [] (Just d) parsed = reverse $ d:parsed
    aux [] Nothing  parsed = reverse parsed

-- Check for an increasing order
isSorted :: (a -> a -> Bool) -> [a] -> Bool
isSorted cmp []     = True
isSorted cmp (x:xs) = aux x xs
  where
    aux _ []      = True
    aux p (x:xs)
      | p `cmp` x = aux x xs
      | otherwise = False

addVec :: Vec -> Vec -> Vec
addVec (a, x) (_, y) = (a, x + y)

cmpVec :: Vec -> Vec -> Bool
cmpVec (a, x) (b, y) = x > y || x == y && a < b

vectorize :: Int -> Vec
vectorize x_i = (x_i, 1)

vecExists :: [Vec] -> Vec -> Bool
vecExists vs v = any (cmp v) vs 
  where cmp (a, _) (b, _) = a == b

main = do
  putStrLn "give me a list of numbers separated by spaces (i.e. '1 2 3'): "
  inp <- getLine 
  print $ run inp

run :: String -> Int
run line = fst 
         $ head 
         $ head 
         $ filter (isSorted cmpVec) 
         $ permutations 
         $ foldl aux [] (map vectorize valid)
  where 
    xs    = parseLine ' ' line
    perms = permutations xs
    valid = head $ filter (isSorted (<=)) perms
    aux vs p@(x_i, k)
      | vecExists vs p = map inc vs 
      | otherwise      = p:vs
      where inc (x, y) = if x == x_i then (x, y + k) else (x, y)