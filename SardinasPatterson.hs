module SardinasPatterson
  ( check,
  )
where

import ListUtil

check :: [String] -> Bool
check code = checkSi s1 [] code
  where
    s1 = filter (/= "") (iPlusOneSet code code)

checkSi :: [String] -> [[String]] -> [String] -> Bool
checkSi ithSet prevSets code
  | "" `elem` ithSet = False
  | nextSet `elem` prevSets = True
  | null nextSet = True
  | otherwise = checkSi nextSet (ithSet : prevSets) code
  where
    nextSet = iPlusOneSet ithSet code

iPlusOneSet :: [String] -> [String] -> [String]
iPlusOneSet ithSet c = danglingSuffixes ithSet c `ListUtil.union` danglingSuffixes c ithSet

danglingSuffixes :: [String] -> [String] -> [String]
danglingSuffixes [] _ = []
danglingSuffixes n d = danglingSuffixes (tail n) d `ListUtil.union` map (ListUtil.stripPrefix (head n)) (filter (ListUtil.isPrefix (head n)) d)
