{-
  Real World Haskell Exercise CIS194
  https://www.seas.upenn.edu/~cis194/fall16/hw/05-real-world-haskell.html
-}
import Data.Char
import Data.Function
import Data.List
import Data.Ord

-- alternate way of expressing lambda
halveEvens :: [Integer] -> [Integer]
halveEvens = map (\x -> x `div` 2) . filter even

safeString :: String -> String
safeString "" = "" -- how to check for empty strings
safeString x = map aptChar x
  where
    aptChar :: Char -> Char
    aptChar c
      | isControl c || not (isAscii c) = '_'
      | otherwise = c

holes :: [a] -> [[a]]
holes [] = []
holes x = map concat $ transpose $ init (inits x) : tail (tails x) : []

longestText :: Show a => [a] -> a
longestText = maximumBy $ comparing (length . show)

-- the code looks slightly look imperative, can be made idiomatic
adjacents :: [a] -> [(a, a)]
adjacents [] = []
adjacents a
  | length a == 1 = []
  | otherwise = (x, y) : adjacents xs
  where
    (x:y:xs) = a

commas :: [String] -> String
commas [] = ""
commas list
  | length list == 1 = head list
  | otherwise = x ++ ", " ++ commas xs
  where
    (x:xs) = list

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials [[]] = []
addPolynomials list = (map sum . transpose) list

sumNumbers :: String -> Integer
sumNumbers "" = 0
sumNumbers str = toInteger $ sum $ map stringToInt $ words $ map addSpaceAtChar str
  where
    addSpaceAtChar :: Char -> Char
    addSpaceAtChar c
      | not (isDigit c) = ' '
      | otherwise = c
    stringToInt :: String -> Int
    stringToInt "" = 0
    stringToInt str = digitToInt (last str) + 10 * stringToInt (init str)
