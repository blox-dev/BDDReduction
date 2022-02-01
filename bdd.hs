import Data.Char

-- Input parsing

-- Remove all spaces from a string
rmspace :: String -> String
rmspace [] = []
rmspace (hd : tl)
  | isSpace hd = rmspace tl
  | True = hd : (rmspace tl)

-- Adds "and" operator according to some rules:
-- Don't place '|' before/after '+', after '(', before ')', after '!'
-- Input: xy+(x)!(!yx)z
-- Output: x|y+(x)|!(!y|x)|z

addand :: String -> String
addand [] = []
addand (c : []) = [c]
addand (c : c2 : tl)
  | c2 == '+' = c : (addand (c2 : tl))
  | c == '+' = c : (addand (c2 : tl))
  | c2 == ')' = c : (addand (c2 : tl))
  | c == '(' = c : (addand (c2 : tl))
  | c == '!' = c : (addand (c2 : tl))
  | True = c : '|' : (addand (c2 : tl))

-- Construct a set of variables with truth values associated in the string
-- Input: xy+(x)!(!yx)z
-- Output: [x,y,z]

vars :: String -> String
vars [] = []
vars (hd : tail)
  | isAlpha hd && (not (hd `elem` tail)) = hd : (vars tail)
  | True = vars tail

-- Shorthand notation for removing spaces and adding 'and' operators to the input
parseInput :: String -> String
parseInput str = addand (rmspace str)

-- Shorthand notation for obtaining the variables from the input
variabile :: String -> String
variabile str = vars (parseInput str)

-- Shorthand notation for how many free variables are in the formula
varlen :: String -> Int
varlen str = length (variabile str)

-- List of tokens
data Token = TVar String | TLParen | TRParen | TAnd | TOr | TNot deriving (Eq, Show)

-- Deconstructs a quanticized formula into tokens
-- Input: (!0|1)|1+0
-- Output: [TLParen, TNot, TVar "0", TAnd, TVar "1", TRParen, TAnd, TVar "1", TOr, TVar "0"]
tokenize :: String -> Maybe [Token]
tokenize [] = Just []
tokenize ('(' : tl) = case tokenize tl of
  Nothing -> Nothing
  Just tl' -> Just (TLParen : tl')
tokenize (')' : tl) = case tokenize tl of
  Nothing -> Nothing
  Just tl' -> Just (TRParen : tl')
tokenize ('+' : tl) = case tokenize tl of
  Nothing -> Nothing
  Just tl' -> Just (TOr : tl')
tokenize ('!' : tl) = case tokenize tl of
  Nothing -> Nothing
  Just tl' -> Just (TNot : tl')
tokenize ('|' : tl) = case tokenize tl of
  Nothing -> Nothing
  Just tl' -> Just (TAnd : tl')
tokenize (c : tl) | isDigit c = case tokenize tl of
  Nothing -> Nothing
  Just tl' -> Just (TVar [c] : tl')
tokenize _ = Nothing

-- Alias for parsing a formula
-- Returns the truth value of a formula
parse_form :: [Token] -> Maybe (Bool, [Token])
parse_form = parse_disjs

-- Parses the disjunctions in a formula
-- And passes the formula further down to parse conjunctions
-- Input: [TVar "0", TOr, TVar "1"]
-- Output: Just (True, [])
parse_disjs :: [Token] -> Maybe (Bool, [Token])
parse_disjs tokens =
  case parse_conjs tokens of
    Nothing -> Nothing
    Just (f1, []) -> Just (f1, [])
    Just (f1, TOr : tokens') ->
      case parse_disjs tokens' of
        Nothing -> Nothing
        Just (f2, tokens'') -> Just ((f1 || f2), tokens'')
    r -> r

-- Parses the conjunctions in a formula
-- And passes the formula further down to parse negations
-- Input: [TVar "0", TAnd, TVar "1"]
-- Output: Just (False, [])
parse_conjs :: [Token] -> Maybe (Bool, [Token])
parse_conjs tokens =
  case parse_negs tokens of
    Nothing -> Nothing
    Just (f1, []) -> Just (f1, [])
    Just (f1, TAnd : tokens') ->
      case parse_conjs tokens' of
        Nothing -> Nothing
        Just (f2, tokens'') -> Just ((f1 && f2), tokens'')
    r -> r

-- Parses the negations in a formula
-- And passes the formula up to parse conjunctions
-- If the formula is incorrect, Nothing will be returned
-- Input: [TNot "1"]
-- Output: Just (False, [])
parse_negs :: [Token] -> Maybe (Bool, [Token])
parse_negs (TVar var : tokens)
  | var == "0" = Just (False, tokens)
  | var == "1" = Just (True, tokens)
  | True = Nothing
parse_negs (TNot : tokens) = case parse_negs tokens of
  Nothing -> Nothing
  Just (f, tokens') -> Just (not f, tokens')
parse_negs (TLParen : tokens) = case parse_form tokens of
  Nothing -> Nothing
  Just (f, TRParen : tokens') -> Just (f, tokens')
  _ -> Nothing
parse_negs _ = Nothing

-- Evaluates the truth value of a quanticized formula
-- By tokenizing a string then evaluating its structure
-- Input: "1|1+!0"
-- Output: Just (True)
parse_formula :: String -> Maybe Bool
parse_formula s = case tokenize s of
  Nothing -> Nothing
  Just tokens -> case parse_form tokens of
    Just (b, []) -> Just b
    _ -> Nothing

-- Binary Decision Diagram data structure
-- Equivalent to a binary tree
data BDD
  = Node String BDD BDD
  | Empty
  deriving (Show, Eq)

-- Replace a char with another one in a string
-- Input: 'a' 'z' "banana"
-- Output: "bznznz"
replace :: Char -> Char -> String -> String
replace x y [] = []
replace x y (hd : tl)
  | x == hd = y : (replace x y tl)
  | True = hd : (replace x y tl)

-- Returns the character at a given index in a string (1-indexed)
-- Input: 3 "abcdefgh"
-- Output: 'c'
letterAtPos :: Int -> String -> Char
letterAtPos m str = head (take 1 (drop (m - 1) str))

-- Build a BDD from a given formula string and the number of its variables+1~
-- Input: 3 "x!y"
-- Output: Node "x!y" (Node "1!y" (Node "1!1" Empty Empty) (Node "1!0" Empty Empty)) (Node "0!y" (Node "0!1" Empty Empty) (Node "0!0" Empty Empty))
-- Can be visualized as: Node "x!y"
--                       /      \
--             Node "1!y"        Node "0!y"
--             /      \          /        \
--    Node "1!1"   Node "1!0"  Node "0!1"  Node "0!0
buildTree :: Int -> String -> BDD
buildTree x str
  | x == 0 = Empty
  | x > 0 = Node str (buildTree (x - 1) (replace (letterAtPos ((varlen str) - x + 1) (variabile str)) '1' str)) (buildTree (x - 1) (replace (letterAtPos ((varlen str) - x + 1) (variabile str)) '0' str))

-- Parses a given BDD in a a bfs manner
-- Input: Node "x!y" (Node "1!y" (Node "1!1" Empty Empty) (Node "1!0" Empty Empty)) (Node "0!y" (Node "0!1" Empty Empty) (Node "0!0" Empty Empty))
-- Output: ["x!y","1!y","0!y","1!1","1!0","0!1","0!0"]
parseBFS :: BDD -> [String]
parseBFS tree = bfs [tree]

bfs :: [BDD] -> [String]
bfs [] = []
bfs trees = (map nodeValue trees) ++ bfs (concat (map childNodes trees))

nodeValue :: BDD -> String
nodeValue (Node a _ _) = a

childNodes :: BDD -> [BDD]
childNodes (Node _ Empty Empty) = []
childNodes (Node _ Empty b) = [b]
childNodes (Node _ a Empty) = [a]
childNodes (Node _ a b) = [a, b]

-- No idea
-- Input: (2 ^ ((varlen formula) + 1) ["x!y","1!y","0!y","1!1","1!0","0!1","0!0"] ["x","y"] 1 1
-- ["x","y","y","0","1","0","0"]

treeToList :: Int -> [String] -> String -> Int -> Int -> Maybe [String]
treeToList _ [] _ _ _ = Just []
treeToList allNodes (hd : tl) vars varIndex index
  | index < (allNodes `div` 2) =
      if (index < (2 ^ varIndex))
        then case (treeToList allNodes tl vars varIndex (index + 1)) of
          Nothing -> Nothing
          Just tail -> Just ([vars !! (varIndex - 1)] : tail)
        else case (treeToList allNodes tl vars (varIndex + 1) (index + 1)) of
          Nothing -> Nothing
          Just tail -> Just ([vars !! (varIndex)] : tail)
  | index < allNodes = case parse_formula hd of
      Nothing -> Nothing
      Just b -> case b of
        True -> case (treeToList allNodes tl vars varIndex (index + 1)) of
          Nothing -> Nothing
          Just tail -> Just ("1" : tail)
        False -> case (treeToList allNodes tl vars varIndex (index + 1)) of
          Nothing -> Nothing
          Just tail -> Just ("0" : tail)
  | True = Just []

-- lista -> BDD

data ListElement = Element {index :: Int, value :: String, leftIndex :: Int, rightIndex :: Int}

instance Show ListElement where
  show Element {index = a, value = b, leftIndex = c, rightIndex = d}
    | c < 0 = (show a) ++ ". " ++ (show b)
    | True = (show a) ++ ". if " ++ (show b) ++ " then " ++ (show c) ++ " else " ++ (show d)

printList :: [ListElement] -> String
printList list = unlines (map show list)

-- turn a list into a [ListElement] idk
-- Input: allNodes, ["x","y","y","0","1","0","0"], ["x","y","y","0","1","0","0"], 1
-- Output: [
--          Element{index = 1, value = "x", leftIndex = 2, rightIndex = 3},
--          Element{index = 2, value = "y", leftIndex = 4, rightIndex = 5},
--          Element{index = 3, value = "y", leftIndex = 4, rightIndex = 4},
--          Element{index = 4, value = "0", leftIndex = -1, rightIndex = -2},
--          Element{index = 1, value = "1", leftIndex = -3, rightIndex = -4}
--         ]
listToBDD :: Int -> [String] -> [String] -> Int -> [ListElement]
listToBDD _ _ [] _ = []
listToBDD allNodes ttl (hd : tl) index
  | index < (allNodes `div` 4) = (Element {index = index, value = hd, leftIndex = 2 * index, rightIndex = 2 * index + 1}) : (listToBDD allNodes ttl tl (index + 1))
  | index < (allNodes `div` 2) = case ttl !! (2 * index - 1) of
      "0" -> case ttl !! (2 * index) of
        "0" -> (Element {index = index, value = hd, leftIndex = (allNodes `div` 2), rightIndex = (allNodes `div` 2)}) : (listToBDD allNodes ttl tl (index + 1))
        "1" -> (Element {index = index, value = hd, leftIndex = (allNodes `div` 2), rightIndex = (allNodes `div` 2) + 1}) : (listToBDD allNodes ttl tl (index + 1))
      "1" -> case ttl !! (2 * index) of
        "0" -> (Element {index = index, value = hd, leftIndex = (allNodes `div` 2) + 1, rightIndex = (allNodes `div` 2)}) : (listToBDD allNodes ttl tl (index + 1))
        "1" -> (Element {index = index, value = hd, leftIndex = (allNodes `div` 2) + 1, rightIndex = (allNodes `div` 2) + 1}) : (listToBDD allNodes ttl tl (index + 1))
  | True = (Element {index = index, value = "0", leftIndex = -1, rightIndex = -2}) : (Element {index = index + 1, value = "1", leftIndex = -3, rightIndex = -4}) : []

-- Elimination rules

-- Returns the element at index n from an element list (1-indexed)
-- Input:  [
--          Element{index = 1, value = "x", leftIndex = 2, rightIndex = 3},
--          Element{index = 2, value = "y", leftIndex = 4, rightIndex = 5},
--          Element{index = 3, value = "y", leftIndex = 4, rightIndex = 4},
--          Element{index = 4, value = "0", leftIndex = -1, rightIndex = -2},
--          Element{index = 1, value = "1", leftIndex = -3, rightIndex = -4}
--         ] 3
-- Output: Element{index = 3, value = "y", leftIndex = 4, rightIndex = 4}
getElem :: Int -> [ListElement] -> ListElement
getElem _ [] = Element {index = -1, value = "", leftIndex = -1, rightIndex = -1}
getElem n (hd : tl)
  | (index hd) == n = hd
  | True = getElem n tl

-- Checks if an element in a list is redundant
-- An element is redundant if its leftIndex is equal to its rightIndex
-- Input: 1 [Element{index=1, value="y", leftIndex = 3, rightIndex = 3}]
-- Output: True
redundantElem :: Int -> [ListElement] -> Bool
redundantElem _ [] = False
redundantElem n list = let elem = (getElem n list) in (leftIndex elem) == (rightIndex elem) || (sameTreeElem (leftIndex elem) (rightIndex elem) list)

-- Checks if two elements describe the same tree
-- Two elements describe the same tree if their leftIndexes are equal and their rightIndexes are equal
-- Input: 1 2 [Element{index=1, value="y", leftIndex = 3, rightIndex = 6}, Element{index=4, value="z", leftIndex = 3, rightIndex = 6},]
-- Output: True
sameTreeElem :: Int -> Int -> [ListElement] -> Bool
sameTreeElem _ _ [] = False
sameTreeElem n m list =
  let elem1 = (getElem n list)
      elem2 = (getElem m list)
   in (leftIndex elem1) == (leftIndex elem2) && (leftIndex elem1) == (leftIndex elem2) && (value elem1) == (value elem2)

-- Replaces an element in a list by removing it and replacing all other elements which refer it to the other input value
-- Input: [Element{index = 1, value = "x", leftIndex = 2, rightIndex = 3},
--          Element{index = 2, value = "y", leftIndex = 4, rightIndex = 5},
--          Element{index = 3, value = "y", leftIndex = 4, rightIndex = 4},
--          Element{index = 4, value = "0", leftIndex = -1, rightIndex = -2},
--          Element{index = 1, value = "1", leftIndex = -3, rightIndex = -4}] 3 4
--
-- Deleting element 3 and replacing all 3's with 4's...
--
-- Output: [Element{index = 1, value = "x", leftIndex = 2, rightIndex = 4},
--          Element{index = 2, value = "y", leftIndex = 4, rightIndex = 5},
--          Element{index = 4, value = "0", leftIndex = -1, rightIndex = -2},
--          Element{index = 1, value = "1", leftIndex = -3, rightIndex = -4}]
replaceElemWith :: [ListElement] -> Int -> Int -> [ListElement]
replaceElemWith [] _ _ = []
replaceElemWith (Element {index = a, value = b, leftIndex = c, rightIndex = d} : tl) x y
  | a == x = replaceElemWith tl x y
  | c /= x && d /= x = (Element {index = a, value = b, leftIndex = c, rightIndex = d}) : (replaceElemWith tl x y)
  | c == x && d /= x = (Element {index = a, value = b, leftIndex = y, rightIndex = d}) : (replaceElemWith tl x y)
  | c /= x && d == x = (Element {index = a, value = b, leftIndex = c, rightIndex = y}) : (replaceElemWith tl x y)
  | c == x && d == x = (Element {index = a, value = b, leftIndex = y, rightIndex = y}) : (replaceElemWith tl x y)

-- Checks if any of the elements in a list are redundant
possibleRedundant :: Int -> [ListElement] -> Bool
possibleRedundant n bdd
  | n <= 0 = False
  | (index (getElem n bdd) /= -1) && (redundantElem n bdd) = True
  | True = possibleRedundant (n - 1) bdd

-- Checks if any pair of elements in a list have the same tree
possibleSameTree :: Int -> Int -> Int -> [ListElement] -> Bool
possibleSameTree m n maxNodes bdd
  | (m <= 0) && (n <= 0) = False
  | m == n = possibleSameTree m (n - 1) maxNodes bdd
  | (m <= 0) = possibleSameTree (m - 1) maxNodes maxNodes bdd
  | (index (getElem m bdd) /= -1) && (index (getElem n bdd) /= -1) && (sameTreeElem m n bdd) = True
  | True = possibleSameTree (m - 1) n maxNodes bdd

-- Check if there are any possible actions left for the user, meaning either there are redundant or sameTree elements left
possibleActions :: Int -> [ListElement] -> Bool
possibleActions allNodes bdd = (possibleRedundant allNodes bdd) || (possibleSameTree allNodes allNodes allNodes bdd)

-- Utility functions

-- Split a string into words
-- Input: "Haskell is great"
-- Output: ["Haskell", "is", "great"]
split :: String -> [String]
split [] = []
split (c : tl)
  | isSpace c = split tl
  | True = (firstWord (c : tl)) : (split (afterFirstWord (c : tl)))

-- Get the first word in a string
-- Input: "Haskell is great"
-- Output: "Haskell"
firstWord :: String -> String
firstWord [] = []
firstWord (c : tl)
  | isAlphaNum c = c : (firstWord tl)
  | True = []

-- Get the string without the first word
-- Input: "Haskell is great"
-- Output: "is great"
afterFirstWord :: String -> String
afterFirstWord [] = []
afterFirstWord (c : tl)
  | isAlphaNum c = afterFirstWord tl
  | True = tl

-- Check if a string represents an integer
-- Input: "10"
-- Output: True
isInteger :: String -> Bool
isInteger [] = True
isInteger (hd : tl)
  | isDigit hd = isInteger tl
  | True = False

-- Checks if an element exists at a certain position in a list(1-indexed)
-- TODO: Maybe the same as getElem
-- Input: 1 [Element{index = 2, value = "y", leftIndex = 4, rightIndex = 5},
--          Element{index = 3, value = "y", leftIndex = 4, rightIndex = 4}]
-- Output: True
isElemInList :: Int -> [ListElement] -> Bool
isElemInList _ [] = False
isElemInList n (Element {index = a, value = b, leftIndex = c, rightIndex = d} : tl)
  | a == n = True
  | True = isElemInList n tl

-- Main loop

main :: IO ()
main = do
  putStrLn "Enter formula:"
  formula <- getLine
  let tree = (buildTree ((varlen formula) + 1) (parseInput formula))
  let allNodes = 2 ^ ((varlen formula) + 1)
  case treeToList allNodes (parseBFS tree) (variabile formula) 1 1 of
    Nothing -> do putStrLn "Invalid input."
    Just ttl -> do
      let bdd = listToBDD allNodes ttl ttl 1
      gameLoop bdd allNodes

gameLoop :: [ListElement] -> Int -> IO ()
gameLoop n allNodes = do
  putStrLn "\nCurrent BDD:"
  putStrLn (printList n)
  putStr "Which transformation to apply?\n"
  input <- getLine
  let words = (split input)
  if (map toLower (words !! 0) == "redundant")
    then
      if (length words /= 2)
        then do
          putStrLn "Wrong usage of 'redundant'. Try 'redundant 1'."
          gameLoop n allNodes
        else do
          case isInteger (words !! 1) of
            False -> do
              putStrLn "Invalid argument. The first argument must be a number."
              gameLoop n allNodes
            True -> do
              let argument = (read (words !! 1) :: Int)
              if (argument < 1 || argument >= (allNodes `div` 2))
                then do
                  putStrLn ("Argument must be between 1 and " ++ (show (allNodes `div` 2 - 1)) ++ ".")
                  gameLoop n allNodes
                else case (isElemInList argument n) of
                  False -> do
                    putStrLn "Element doesn't exist."
                    gameLoop n allNodes
                  True -> case (redundantElem argument n) of
                    False -> do
                      putStrLn "Selected node is not redundant."
                      gameLoop n allNodes
                    True -> do
                      let n2 = (replaceElemWith n argument (leftIndex (getElem argument n)))
                      gameLoop n2 allNodes
    else
      if (map toLower (words !! 0) == "sametree")
        then
          if (length words /= 3)
            then do
              putStrLn "Wrong usage of 'sameTree'. Try 'sameTree 1 2'."
              gameLoop n allNodes
            else do
              case (isInteger (words !! 1) && isInteger (words !! 2)) of
                False -> do
                  putStrLn "Invalid arguments. The first and second arguments must be numbers."
                  gameLoop n allNodes
                True -> do
                  let argument1 = (read (words !! 1) :: Int)
                      argument2 = (read (words !! 2) :: Int)
                  if (argument1 == argument2)
                    then do
                      putStrLn "Cannot use sameTree rule on the same argument twice."
                      gameLoop n allNodes
                    else
                      if (argument1 < 1 || argument1 >= (allNodes `div` 2) || argument2 < 1 || argument2 >= (allNodes `div` 2))
                        then do
                          putStrLn ("Arguments must be between 1 and " ++ (show (allNodes `div` 2 - 1)))
                          gameLoop n allNodes
                        else case ((isElemInList argument1 n) && (isElemInList argument2 n)) of
                          False -> do
                            putStrLn "Element doesn't exist."
                            gameLoop n allNodes
                          True -> case (sameTreeElem argument1 argument2 n) of
                            False -> do
                              putStrLn "Selected nodes don't respect the sameTree rule."
                              gameLoop n allNodes
                            True -> do
                              let n2 = (replaceElemWith n argument2 argument1)
                              gameLoop n2 allNodes
        else
          if ((length words == 1) && (map toLower (words !! 0)) == "done")
            then
              if (possibleActions (allNodes `div` 2 - 1) n)
                then do
                  putStrLn "BDD has possible transformations left."
                  gameLoop n allNodes
                else do putStrLn ("Reduced BDD has " ++ (show (length n)) ++ " nodes.\n")
            else
              if ((length words == 1) && (map toLower (words !! 0)) == "donef")
                then do
                  putStrLn "Exiting current iteration."
                  putStrLn ("Last BDD had " ++ (show (length n)) ++ " nodes.\n")
                else do
                  putStrLn "Invalid command. Try one of the following:"
                  putStrLn "-> redundant x"
                  putStrLn "-> sameTree x y"
                  putStrLn "-> done"
                  gameLoop n allNodes