import Data.Char;

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

input = "xy+xyz"
copac = (buildTree ((varlen input)+1) (parseInput input))
copacBF = traverseBF copac

--bfTree -> lista

allNodes = 2 ^ ((varlen input) + 1)
halfNodes = allNodes `div` 2
quarterNodes = halfNodes `div` 2

-- TODO: Nothing handling

treeToList :: [String] -> Int -> Int -> [String]
treeToList [] _ _ = []
treeToList (hd:tl) varIndex index | index < halfNodes = if(index < (2^varIndex)) then [(vars input)!!(varIndex-1)]:(treeToList tl varIndex (index+1)) else [(vars input)!!(varIndex)]:(treeToList tl (varIndex+1) (index+1)) 
                                  | index < allNodes = case parse_formula hd of
                                                            Nothing -> "bruh":(treeToList tl varIndex (index+1))
                                                            Just b -> case b of
                                                                True -> "1":(treeToList tl varIndex (index+1))
                                                                False -> "0":(treeToList tl varIndex (index+1))
                                  | True = []

ttl = treeToList copacBF 1 1

-- lista -> BDD

data ListElement = Element { index::Int, value::String, leftIndex::Int, rightIndex::Int}

instance Show ListElement where
    show Element {index = a, value = b, leftIndex = c, rightIndex = d} | c < 0 = (show a) ++ ". " ++ (show b) ++ "\n"
                                                                       | True = (show a) ++ ". if " ++ (show b) ++ " then " ++ (show c) ++ " else " ++ (show d) ++ "\n"

l1= Element {index=1,value="bruh",leftIndex=3,rightIndex=4}


listToBDD :: [String] -> Int -> [ListElement]
listToBDD [] _ = []
listToBDD (hd:tl) index | index < quarterNodes = (Element {index = index, value = hd, leftIndex = 2*index, rightIndex = 2*index + 1}):(listToBDD tl (index+1))
                        | index < halfNodes = case ttl!!(2*index-1) of
                                                "0" -> case ttl!!(2*index) of
                                                            "0" -> (Element {index = index, value = hd, leftIndex = halfNodes, rightIndex = halfNodes}):(listToBDD tl (index+1))
                                                            "1" -> (Element {index = index, value = hd, leftIndex = halfNodes, rightIndex = halfNodes +1}):(listToBDD tl (index+1))
                                                "1" -> case ttl!!(2*index) of
                                                            "0" -> (Element {index = index, value = hd, leftIndex = halfNodes +1, rightIndex = halfNodes}):(listToBDD tl (index+1))
                                                            "1" -> (Element {index = index, value = hd, leftIndex = halfNodes +1, rightIndex = halfNodes +1}):(listToBDD tl (index+1))
                        | True = (Element {index = index, value = "0", leftIndex = -1, rightIndex = -2}):(Element {index = index+1, value = "1", leftIndex = -3, rightIndex = -4}):[]

bdd = listToBDD ttl 1

--reguli eliminare

redundantElem :: ListElement -> Bool
redundantElem (Element{index = _, value = _, leftIndex = a, rightIndex = b}) = if(a == b) then True else False

redundant n = redundantElem (bdd!!(n-1))

sameTreeElem :: ListElement -> ListElement -> Bool
sameTreeElem (Element{index = _, value = _, leftIndex = a, rightIndex = b}) (Element{index = _, value = _, leftIndex = c, rightIndex = d}) = if(a==c && b == d) then True else False

sameTree m n = sameTreeElem (bdd!!(m-1)) (bdd!!(n-1)) 

--main

-- main :: IO ()
-- main = do putStrLn "Introduceti o formula logica: "
--           fs <- getLine
--           case buildTree (varlen fs) (parseInput fs) of
--               Just (bdd) -> do putStrLn $ "Ati introdus formula: " ++ (show fs)
--                                putStrLn $ "Copacul este: " ++ (show bdd)
--                                main
--               _ -> do putStrLn "Eroare de sintaxa."