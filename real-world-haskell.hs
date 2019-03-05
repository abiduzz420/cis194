{-
  Real World Haskell Exercise CIS194
  https://www.seas.upenn.edu/~cis194/fall16/hw/05-real-world-haskell.html
-}
import Data.Char
import Data.Function
import Data.List
import Data.Ord

-- Exercise 1
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
  | otherwise = (x, y) : adjacents (y:xs)
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
sumNumbers str =
  toInteger $ sum $ map stringToInt $ words $ map addSpaceAtChar str
  where
    addSpaceAtChar :: Char -> Char
    addSpaceAtChar c
      | not (isDigit c) = ' '
      | otherwise = c
    stringToInt :: String -> Int
    stringToInt "" = 0
    stringToInt str = digitToInt (last str) + 10 * stringToInt (init str)

-- Exercise 2
contains :: String -> [String] -> Bool
contains _ [] = False
contains str (x:xs) = str == x || contains str xs

unique :: [String] -> [String] -> [String]
unique uniqueList list
  | list == [] = uniqueList
  | not (contains x uniqueList) = unique (x : uniqueList) xs
  | otherwise = unique uniqueList xs
  where
    (x:xs) = list

wordCount :: String -> String
wordCount str =
  unlines
    [ "Number of lines: " ++ (show numLines)
    , "Number of emptylines: " ++ (show emptyLines)
    , "Number of words: " ++ (show numWords)
    , "Number of unique words: " ++ (show uniqueWords)
    , "Number of words followed by themselves: " ++ (show sameWords)
    , "Length of the longest line: " ++ (show longestLine)
    ]
  where
    allLines = lines str
    numLines = length allLines
    emptyLines = (length . filter (== "")) allLines
    allWords = words str
    numWords = length allWords
    uniqueWords = length $ unique [] allWords
    -- Alternatively you can just do `length $ nub allWords to find uniqueWords`
    sameWords = (length . filter (\(x, y) -> x == y) . adjacents) allWords
    longestLine = longestText allLines

-- Exercise 3
testResults :: [(String, [Bool])]
testResults =
  [ ("halveEvens", ex_halveEvens)
  , ("safeString", ex_safeString)
  , ("holes", ex_holes)
  , ("longestText", ex_longestText)
  , ("adjacents", ex_adjacents)
  , ("commas", ex_commas)
  , ("addPolynomials", ex_addPolynomials)
  , ("sumNumbers", ex_sumNumbers)
  ]

formatTest :: (String, [Bool]) -> String
formatTest (x,list) = 
  x ++ ": "
  ++ (show . length . filter (\x -> x == True)) list
  ++ "/" ++ (show . length) list ++ " passed. "
  ++ failTests list

formatTests :: [(String, [Bool])] -> String
formatTests tests =
  (unlines . map formatTest) tests

failTests :: [Bool] -> String
failTests boolList
  | and boolList = "✔"
  | otherwise = "Failed " ++ (commas . map show . map succ . elemIndices False) boolList

main :: IO()
main = putStrLn $ formatTests testResults

  -- Test cases
ex_halveEvens =
  [ halveEvens [] == []
  , halveEvens [1, 2, 3, 4, 5] == [1, 2]
  , halveEvens [6, 6, 6, 3, 3, 3, 2, 2, 2] == [3, 3, 3, 1, 1, 1]
  ]

ex_safeString =
  [ safeString [] == []
  , safeString "Hello World!" == "Hello World!"
  , safeString "That🙋s your line:\n" == "That_s your line:_"
  , safeString "🙋.o(🙋Me Me Me🙋)" == "_.o(_Me Me Me_)"
  ]

ex_holes =
  [holes "" == [], holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]]

ex_longestText =
  [ longestText [True, False] == False
  , longestText [2, 4, 16, 32] == (32 :: Int)
  , longestText (words "Hello World") == "World"
  , longestText (words "OlÃ¡ mundo") == "OlÃ¡"
  ]

ex_adjacents =
  [ adjacents "" == []
  , adjacents [True] == []
  , adjacents "Hello" == [('H', 'e'), ('e', 'l'), ('l', 'l'), ('l', 'o')]
  , adjacents "Hell" == [('H', 'e'), ('e', 'l'), ('l', 'l')]
  ]

ex_commas =
  [ commas [] == ""
  , commas ["Hello"] == "Hello"
  , commas ["Hello", "World"] == "Hello, World"
  , commas ["Hello", "", "World"] == "Hello, , World"
  , commas ["Hello", "new", "World"] == "Hello, new, World"
  ]

ex_addPolynomials =
  [ addPolynomials [[]] == []
  , addPolynomials [[0, 1], [1, 1]] == [1, 2]
  , addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
  ]

ex_sumNumbers =
  [ sumNumbers "" == 0
  , sumNumbers "Hello world!" == 0
  , sumNumbers "a1bc222d3f44" == 270
  , sumNumbers "words0are1234separated12by3integers45678" == 46927
  , sumNumbers "000a." == 0
  , sumNumbers "0.00a." == 0
  ]