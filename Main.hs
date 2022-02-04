module Main
  ( main,
  )
where

import ListUtil
import Regex
import SardinasPatterson
import System.IO

main :: IO ()
main = do
  putStrLn "Enter regular expression to check:"
  inputRegex <- getLine
  let generated = ListUtil.removeDuplicates (Regex.generateStrings (Regex.parse inputRegex))
  putStrLn ("Checking word set: " ++ show generated)
  showResult (SardinasPatterson.check generated)

showResult :: Bool -> IO ()
showResult isCode = do
  if isCode then putStrLn "Given word set IS code." else putStrLn "Given word set CANNOT be code."
