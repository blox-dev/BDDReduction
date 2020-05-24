import Data.Char;
import Data.List;

--parsare input 

rmspace :: String -> String
rmspace [] = []
rmspace (hd:tl) | isSpace hd = rmspace tl
                | True = hd:(rmspace tl)

--"xy+(x)!(!yx)z" (0)(0)
-- nu pun |
-- inainte de +
-- dupa +
-- inainte )
-- dupa (
-- dupa !

addand :: String -> String
addand [] = []
addand (c:[]) = [c]
addand (c:c2:tl) | c2 == '+' = c:(addand (c2:tl))
                | c == '+' = c:(addand (c2:tl))
                | c2 == ')' = c:(addand (c2:tl))
                | c == '(' = c:(addand (c2:tl))
                | c == '!' = c:(addand (c2:tl))
                | True = c:'|':(addand (c2:tl))

vars :: String -> String
vars [] = []
vars (hd:tail) | isAlpha hd && (not (hd `elem` tail)) = hd:(vars tail)
               | True = vars tail

parseInput str = addand (rmspace str)
variabile str = vars (parseInput str)
varlen str = length (variabile str)
        
--tokenizare

data Token = TVar String | TLParen | TRParen | TAnd | TOr | TNot deriving (Eq, Show)

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

-- parsare

parse_form = parse_disjs


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

parse_negs :: [Token] -> Maybe (Bool, [Token])
parse_negs (TVar var : tokens) | var == "0" = Just (False, tokens)
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

parse_formula :: String -> Maybe Bool
parse_formula s = case tokenize s of
                    Nothing -> Nothing
                    Just tokens -> case parse_form tokens of
                        Just(b,[]) -> Just b
                        _ -> Nothing

--construire arbore

data BDD = Node String BDD BDD
         | Empty
         deriving (Show,Eq)

replace :: Char -> Char -> String -> String
replace x y [] = []
replace x y (hd:tl) | x == hd = y:(replace x y tl)
                    | True = hd:(replace x y tl)

letterAtPos :: Int -> String -> Char
letterAtPos m str = head (take 1 (drop (m-1) str))

buildTree :: Int -> String -> BDD
buildTree x str
    | x == 0 = Empty
    | x > 0 = Node str (buildTree (x-1) (replace (letterAtPos ((varlen str)-x+1) (variabile str)) '1' str)) (buildTree (x-1) (replace (letterAtPos ((varlen str)-x+1) (variabile str)) '0' str))

--bfs arbore

traverseBF tree = tbf [tree]

tbf :: [BDD] -> [String]
tbf [] = []
tbf xs = (map nodeValue xs) ++ tbf (concat (map leftAndRightNodes xs))

nodeValue :: BDD -> String
nodeValue (Node a _ _) = a

leftAndRightNodes :: BDD -> [BDD]
leftAndRightNodes (Node _ Empty Empty) = []
leftAndRightNodes (Node _ Empty b)     = [b]
leftAndRightNodes (Node _ a Empty)     = [a]
leftAndRightNodes (Node _ a b)       = [a,b]

--bfTree -> lista

treeToList :: Int -> [String] -> String -> Int -> Int -> Maybe [String]
treeToList _ [] _ _ _= Just []
treeToList allNodes (hd:tl) vars varIndex index | index < (allNodes `div` 2) =  if(index < (2^varIndex)) then
                                                                                    case (treeToList allNodes tl vars varIndex (index+1)) of
                                                                                        Nothing -> Nothing
                                                                                        Just tail -> Just ([vars!!(varIndex-1)]:tail) 
                                                                                else case (treeToList allNodes tl vars (varIndex+1) (index+1)) of
                                                                                        Nothing -> Nothing
                                                                                        Just tail -> Just([vars!!(varIndex)]:tail)
                                                | index < allNodes = case parse_formula hd of
                                                            Nothing -> Nothing
                                                            Just b -> case b of
                                                                True -> case (treeToList allNodes tl vars varIndex (index+1)) of
                                                                            Nothing -> Nothing
                                                                            Just tail -> Just ("1":tail)
                                                                False -> case (treeToList allNodes tl vars varIndex (index+1)) of
                                                                            Nothing -> Nothing
                                                                            Just tail -> Just ("0":tail)
                                                | True = Just []

--ttl = treeToList copacBF 1 1

-- lista -> BDD

data ListElement = Element { index::Int, value::String, leftIndex::Int, rightIndex::Int}

instance Show ListElement where
    show Element {index = a, value = b, leftIndex = c, rightIndex = d} | c < 0 = (show a) ++ ". " ++ (show b)
                                                                       | True = (show a) ++ ". if " ++ (show b) ++ " then " ++ (show c) ++ " else " ++ (show d)
    showList list = ((intercalate "\n" (map show list)) ++)

listToBDD :: Int -> [String] -> [String] -> Int -> [ListElement]
listToBDD _ _ [] _ = []
listToBDD allNodes ttl (hd:tl) index | index < (allNodes `div` 4) = (Element {index = index, value = hd, leftIndex = 2*index, rightIndex = 2*index + 1}):(listToBDD allNodes ttl tl (index+1))
                                     | index < (allNodes `div` 2) = case ttl!!(2*index-1) of
                                                             "0" -> case ttl!!(2*index) of
                                                                         "0" -> (Element {index = index, value = hd, leftIndex = (allNodes `div` 2), rightIndex = (allNodes `div` 2)}):(listToBDD allNodes ttl tl (index+1))
                                                                         "1" -> (Element {index = index, value = hd, leftIndex = (allNodes `div` 2), rightIndex = (allNodes `div` 2) +1}):(listToBDD allNodes ttl tl (index+1))
                                                             "1" -> case ttl!!(2*index) of
                                                                         "0" -> (Element {index = index, value = hd, leftIndex = (allNodes `div` 2) +1, rightIndex = (allNodes `div` 2)}):(listToBDD allNodes ttl tl (index+1))
                                                                         "1" -> (Element {index = index, value = hd, leftIndex = (allNodes `div` 2) +1, rightIndex = (allNodes `div` 2) +1}):(listToBDD allNodes ttl tl (index+1))
                                     | True = (Element {index = index, value = "0", leftIndex = -1, rightIndex = -2}):(Element {index = index+1, value = "1", leftIndex = -3, rightIndex = -4}):[]


--reguli eliminare

redundantElem :: Int -> [ListElement] -> Bool
redundantElem _ [] = False
redundantElem n list = let elem = (getElem n list) in (leftIndex elem) == (rightIndex elem)

sameTreeElem :: Int -> Int -> [ListElement] -> Bool
sameTreeElem _ _ [] = False
sameTreeElem n m list = let elem1 = (getElem n list)
                            elem2 = (getElem m list) in (leftIndex elem1) == (leftIndex elem2) && (leftIndex elem1) == (leftIndex elem2) && (value elem1) == (value elem2)

replaceElemWith :: [ListElement] -> Int -> Int -> [ListElement]
replaceElemWith [] _ _ = []
replaceElemWith (Element{index = a, value = b, leftIndex = c, rightIndex = d}:tl) x y | a == x = replaceElemWith tl x y
                                                                                      | c /= x && d /=x = (Element{index = a, value = b, leftIndex = c, rightIndex = d}):(replaceElemWith tl x y)
                                                                                      | c == x && d /=x = (Element{index = a, value = b, leftIndex = y, rightIndex = d}):(replaceElemWith tl x y)
                                                                                      | c /= x && d ==x = (Element{index = a, value = b, leftIndex = c, rightIndex = y}):(replaceElemWith tl x y)
                                                                                      | c == x && d ==x = (Element{index = a, value = b, leftIndex = y, rightIndex = y}):(replaceElemWith tl x y)

possibleActions :: Int -> [ListElement] -> Bool
possibleActions allNodes bdd = (possibleRedundant allNodes bdd) || (possibleSameTree allNodes allNodes allNodes bdd)

possibleRedundant :: Int -> [ListElement] -> Bool
possibleRedundant n bdd | n <= 0 = False
                        | (index (getElem n bdd) /= -1) && (redundantElem n bdd) = True
                        | True = possibleRedundant (n-1) bdd

possibleSameTree :: Int -> Int -> Int -> [ListElement] -> Bool
possibleSameTree m n maxNodes bdd | (m <= 0) && (n <= 0) = False
                                  | m == n = possibleSameTree m (n-1) maxNodes bdd
                                  | (m <= 0) = possibleSameTree (m-1) maxNodes maxNodes bdd
                                  | (index (getElem m bdd) /= -1) && (index (getElem n bdd) /= -1) && (sameTreeElem m n bdd) = True
                                  | True = possibleSameTree (m-1) n maxNodes bdd

-- utils

split :: String -> [String]
split [] = []
split (c:tl) | isSpace c = split tl
             | True = (firstWord (c:tl)):(split (afterFirstWord (c:tl))) 

firstWord :: String -> String
firstWord [] = []
firstWord (c:tl) | isAlphaNum c = c:(firstWord tl)
                 | True = []

afterFirstWord :: String -> String
afterFirstWord [] = []
afterFirstWord (c:tl) | isAlphaNum c = afterFirstWord tl
                      | True = tl

isInteger :: String -> Bool
isInteger [] = True
isInteger (hd:tl) | isDigit hd = isInteger tl
                  | True = False

isElemInList :: Int -> [ListElement] -> Bool
isElemInList _ [] = False
isElemInList n (Element{index = a, value = b, leftIndex = c, rightIndex = d}:tl) | a == n = True
                                                                                 | True = isElemInList n tl

getElem :: Int -> [ListElement] -> ListElement
getElem _ [] = Element{index = -1, value = "", leftIndex = -1, rightIndex = -1}
getElem n (hd:tl) | (index hd) == n = hd
                  | True = getElem n tl
--main + loop

main :: IO ()
main = do putStr "Enter formula:\n-> "
          formula <- getLine
          let tree = (buildTree ((varlen formula)+1) (parseInput formula))
          let allNodes = 2 ^ ((varlen formula) + 1)
          case treeToList allNodes (traverseBF tree) (variabile formula) 1 1 of
              Nothing -> do putStrLn "Invalid input"
              Just ttl -> do let bdd = listToBDD allNodes ttl ttl 1
                             gameLoop bdd allNodes

gameLoop :: [ListElement] -> Int -> IO ()
gameLoop n allNodes = do putStrLn "\nCurrent BDD:"
                         print n
                         putStr "\nWhich transformation to apply?\n-> "
                         input <- getLine
                         putStr "\n"
                         let words = (split input)
                         if(map toLower (words!!0) == "redundant") then 
                             if(length words /= 2) then do putStrLn "Wrong usage of 'redundant'. Try 'redundant 1'."
                                                           gameLoop n allNodes
                             else do case isInteger (words!!1) of
                                       False -> do putStrLn "Invalid argument."
                                                   gameLoop n allNodes
                                       True -> do let argument = (read (words!!1) :: Int)
                                                  if(argument<1 || argument >= (allNodes `div` 2)) 
                                                      then do putStrLn ("Argument must be between 1 and " ++ (show (allNodes `div` 2-1)) ++ ".")
                                                              gameLoop n allNodes
                                                  else case (isElemInList argument n) of
                                                         False -> do putStrLn "Element doesn't exist."
                                                                     gameLoop n allNodes
                                                         True -> case (redundantElem argument n) of
                                                                   False -> do putStrLn "Selected cause is not redundant."
                                                                               gameLoop n allNodes
                                                                   True -> do let n2 = (replaceElemWith n argument (leftIndex (getElem argument n)))
                                                                              gameLoop n2 allNodes

                         else if(map toLower (words!!0) == "sametree") then
                                  if(length words /= 3) then do putStrLn "Wrong usage of 'sameTree'. Try 'sameTree 1 2'."
                                                                gameLoop n allNodes 
                                  else do case (isInteger (words!!1) && isInteger (words!!2)) of
                                            False -> do putStrLn "Invalid arguments."
                                                        gameLoop n allNodes
                                            True -> do let argument1 = (read (words!!1) :: Int)
                                                           argument2 = (read (words!!2) :: Int)
                                                       if(argument1 == argument2)
                                                           then do putStrLn "Cannot use sameTree rule on the same argument twice."
                                                                   gameLoop n allNodes
                                                       else if(argument1 < 1 || argument1 >= (allNodes `div` 2) || argument2 < 1 || argument2 >= (allNodes `div` 2))
                                                                then do putStrLn ("Arguments must be between 1 and " ++ (show (allNodes `div` 2-1)))
                                                                        gameLoop n allNodes
                                                            else case ((isElemInList argument1 n) && (isElemInList argument2 n)) of
                                                                   False -> do putStrLn "Element doesn't exist."
                                                                               gameLoop n allNodes
                                                                   True -> case (sameTreeElem argument1 argument2 n) of
                                                                             False -> do putStrLn "Selected causes are not sameTree lol."
                                                                                         gameLoop n allNodes
                                                                             True -> do let n2 = (replaceElemWith n argument2 argument1)
                                                                                        gameLoop n2 allNodes


                         else if ((length words == 1) && (map toLower (words!!0)) == "done")
                             then if (possibleActions (allNodes `div` 2 - 1) n) 
                                    then do putStrLn "BDD still has possible moves."
                                            gameLoop n allNodes
                                  else do putStrLn ("Reduced BDD has " ++ (show (length n)) ++ " nodes.\n")

                         else do putStrLn "Invalid command. Try one of the following:"
                                 putStrLn "-> redundant x"
                                 putStrLn "-> sameTree x y"
                                 putStrLn "-> done"
                                 gameLoop n allNodes