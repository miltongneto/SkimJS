module Value (Value (..)) where

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Nil
    | Stop
    | Continue
    | Retorno
    | Lista [Value]
    deriving (Eq)
 
--
-- Pretty Printer
--


instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show Nil = "undefined"
  show (Lista l) = "[" ++ (showLista (Lista l)) ++ "]"

showLista :: Value -> String
showLista (Lista []) = ""
showLista (Lista [b]) = show b
showLista (Lista (b:bs)) = (show b) ++ ", " ++ showLista (Lista bs)


--instance Eq Value where
--  (Bool b1) == (Bool b2) = b1 == b2
--  (Int x) == (Int y) = x == y 
--  (String str1) == (String str2) = str1 == str2
--  (Var x) == (Var y) = x == y
--  Nil == Nil = True
--  _ == _ = False
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
