{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module WebRepl.Expr where

import           Protolude

import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import qualified Data.Map                  as Map

import           WebRepl.Exn

type Program = [Stmt]

data Stmt
    = Assign Text Expr
    | Print Expr

data Expr
    = LInt Integer
    | Add Expr Expr
    | Var Text

type EvalState = Map Text Integer

evalExpr
    :: (MonadError ServiceError m, MonadState EvalState m)
    => Expr
    -> m Integer
evalExpr (LInt a) =
    pure a
evalExpr (Add a b) =
    liftA2 (+) (evalExpr a) (evalExpr b)
evalExpr (Var txt) =
    (`orThrow` EvalError (VarNotFound txt)) =<< gets (Map.lookup txt)

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow (Just a) _ = pure a
orThrow Nothing  e = throwError e
