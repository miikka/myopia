{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Myopia.AST where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.RWS
import           Data.Functor.Identity (Identity)
import           Data.Map              (Map)
import qualified Data.Map              as SM
import           Data.Maybe            (fromJust)

data Expr = Z
          | S
          | I Int Integer
          | C Fun [Fun]
          | P Fun Fun
          | M Fun
          | FC FunName
          deriving (Eq, Show)

type FunName = String
type Arity = Integer
type Fun = Expr
type Builtin m = [Integer] -> m Integer
type BuiltinMap m = Map FunName (Builtin m)

data Program = Program
    { _funDefs  :: Map FunName Fun
    , _typeDefs :: Map FunName Arity
    } deriving (Show)

makeLenses ''Program

instance Monoid Program where
    mempty = Program mempty mempty
    mappend (Program a1 b1) (Program a2 b2) = Program (a1 <> a2) (b1 <> b2)

data Env m = Env
    { _program  :: Program
    , _builtins :: BuiltinMap m
    }

makeLenses ''Env

emptyEnv = Env { _program = mempty, _builtins = mempty }

builtin :: FunName -> Lens' (Env m) (Maybe (Builtin m))
builtin fn = builtins.at fn

type MemoMap = SM.Map (FunName, [Integer]) Integer
type MyopiaT m = RWST (Env m) () MemoMap m
type MyopiaM = MyopiaT Identity

funDef :: FunName -> Lens' (Env m) (Maybe Fun)
funDef fn = program.funDefs.at fn

typeDef :: FunName -> Lens' (Env m) (Maybe Arity)
typeDef fn = program.typeDefs.at fn

getDef :: (Functor m, Monad m) => FunName -> MyopiaT m Expr
getDef fn = view (funDef fn) >>= \x -> case x of
    Nothing -> error $ "Function " ++ fn ++ " not found."
    Just def -> return def

runMyopiaM :: MyopiaM a -> Program -> a
runMyopiaM f prog = fst $ evalRWS f (emptyEnv & program .~ prog) SM.empty

runMyopiaT :: (Functor m, Monad m) => MyopiaT m a -> Program -> m a
runMyopiaT f prog = fst <$> evalRWST f (emptyEnv & program .~ prog) SM.empty

runMyopiaT' :: (Functor m, Monad m) => BuiltinMap m -> MyopiaT m a -> Program -> m a
runMyopiaT' bm f prog = fst <$> evalRWST f (Env { _program = prog, _builtins = bm }) SM.empty

arityM :: (Functor m, Monad m) => Expr -> MyopiaT m Integer
arityM Z = return 1
arityM S = return 1
arityM (I _ k) = return k
arityM (C _ gs) = arityM (head gs)
arityM (P g _) = succ <$> arityM g
arityM (M f) = pred <$> arityM f
arityM (FC fn) = getDef fn >>= arityM

getArity :: Program -> FunName -> Integer
getArity prog fn = runMyopiaM (getDef fn >>= arityM) prog

-- vim: set ts=4 sw=4 et
