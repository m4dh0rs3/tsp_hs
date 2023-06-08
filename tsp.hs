import Data.Ord (comparing)
import Data.List (minimumBy)

path i d [] = (d !! i !! 0, [0])
path i d l = minimumBy (comparing fst)
  [(\(d', p) -> (d' + (d !! i !! j), j:p))
    (path j d (filter (\e -> e /= j) l))
   | j <- l]

tsp d = (\(d', p) -> (d', 0:p)) $ path 0 k [1..((length d) - 1)]

-- 0-2-3-1-0 or 0-1-3-2-0
k = [[0, 4, 1, 7],
     [5, 0, 2, 6],
     [2, 1, 0, 3],
     [7, 5, 2, 0]]

main = do
  putStrLn $ show $ tsp k
  