module AST where

type Ident = String

type TypeHint = Maybe String

data AST
  = AIntL Int
  | ACall Ident
  | ADefn Ident [AST]
  | ALamb [AST]
  | AComm String
  deriving (Show)

-- instance Show AST where
--   show (AIntL i) = show i
--   show (ACall n) = "@" ++ n
--   show (AComm s) = "# " ++ s
--   show (ADefn n body) = ": " ++ n ++ " " ++ foldr (\x acc -> if null acc then show x else show x ++ " " ++ acc) "" body ++ " ;"
--   show (ALamb body) = "[" ++ foldr (\x acc -> if null acc then show x else show x ++ " " ++ acc) "" body ++ "]"

type Program = [AST]
