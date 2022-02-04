module Regex
  ( generateStrings,
    parse,
  )
where

import ListUtil

data Regex = Epsilon | Single Char | Seq Regex Regex | Or Regex Regex | Star Regex deriving (Show)

generateStrings :: Regex -> [String]
generateStrings Epsilon = [""]
generateStrings (Single x) = [[x]]
generateStrings (Or r1 r2) = ListUtil.interleave (generateStrings r1) (generateStrings r2)
generateStrings (Seq r1 r2) = [s1 ++ s2 | (s1, s2) <- ListUtil.cross (generateStrings r1) (generateStrings r2)]
generateStrings (Star r) = take 100 (generateStrings Epsilon ++ generateStrings (Seq r (Star r)))

parse :: String -> Regex
parse [] = Epsilon
parse ['.'] = Epsilon
parse [x] = Single x
parse x
  | last x == '*' && l == length x = Star (parse (init x))
  | head x == '(' && last x == ')' && l == length x = parse (tail (init x))
  | head afterBracket == '+' = Or (parse insideBracket) (parse (tail afterBracket))
  | otherwise = Seq (parse insideBracket) (parse afterBracket)
  where
    l = getBracketsLengthWithStar x
    insideBracket = take l x
    afterBracket = drop l x

getBracketsLengthWithStar :: String -> Int
getBracketsLengthWithStar x = l + length (takeWhile (== '*') (drop l x))
  where
    l = getBracketsLength x

getBracketsLength :: String -> Int
getBracketsLength x
  | null x = 0
  | head x /= '(' = 1
  | otherwise = length (takeWhile (/= 0) (scanl countOpenBrackets 1 (tail x))) + 1

countOpenBrackets :: Int -> Char -> Int
countOpenBrackets acc x
  | x == '(' = acc + 1
  | x == ')' = acc - 1
  | otherwise = acc
