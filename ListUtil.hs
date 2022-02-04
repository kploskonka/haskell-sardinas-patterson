module ListUtil
  ( interleave,
    cross,
    removeDuplicates,
    union,
    isPrefix,
    stripPrefix,
  )
where

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x : xs) ys = x : interleave ys xs

cross :: [a] -> [b] -> [(a, b)]
cross [] ys = []
cross xs ys = foldr (\x -> interleave ([(x, y) | y <- ys])) [] xs

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, y `notElem` xs]

prefixes :: [a] -> [[a]]
prefixes = scanl (\acc x -> acc ++ [x]) []

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix prefix xs = prefix `elem` prefixes xs

stripPrefix :: (Eq a) => [a] -> [a] -> [a]
stripPrefix _ [] = []
stripPrefix [] xs = xs
stripPrefix (x:xs) (y:ys) = if x == y then stripPrefix xs ys else []
