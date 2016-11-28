module WebRepl.Expr where

import           Protolude

import qualified Data.Map  as Map

type Program = [Stmt]

data Stmt
    = Assign Text Expr
    | Print Expr

data Expr
    = LInt Integer
    | Add Expr Expr
    | Var Text

evalExpr :: Expr -> Map Text Integer -> Maybe Integer
evalExpr (LInt a)  _    = pure a
evalExpr (Add a b) m    = liftA2 (+) (evalExpr a m) (evalExpr b m)
evalExpr (Var txt) vars = Map.lookup txt vars
