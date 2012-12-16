{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Myopia.AST where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.RWS
import              Data.Functor.Identity (Identity)
import           Data.Map            (Map)
import qualified Data.Map            as M
import qualified Data.Map            as SM
--import qualified Data.Map.Strict     as SM
import           Data.Maybe          (fromJust)

import           Debug.Trace

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
    { _program :: Program
    , _builtins :: BuiltinMap m
    }

makeLenses ''Env

emptyEnv = Env { _program = mempty, _builtins = mempty }

builtin :: FunName -> SimpleLens (Env m) (Maybe (Builtin m))
builtin fn = builtins.at fn

type MemoMap = SM.Map (FunName, [Integer]) Integer
type MyopiaT m = RWST (Env m) () MemoMap m
type MyopiaM = MyopiaT Identity

arityM :: (Functor m, Monad m) => Expr -> MyopiaT m Integer
arityM Z = return 1
arityM S = return 1
arityM (I _ k) = return k
arityM (C _ gs) = arityM (head gs)
arityM (P g _) = succ <$> arityM g
arityM (M f) = pred <$> arityM f
arityM (FC fn) = getDef fn >>= arityM

funDef :: FunName -> SimpleLens (Env m) (Maybe Fun)
funDef fn = program.funDefs.at fn

typeDef :: FunName -> SimpleLens (Env m) (Maybe Arity)
typeDef fn = program.typeDefs.at fn

getDef :: (Functor m, Monad m) => FunName -> MyopiaT m Expr
getDef fn = fromJust <$> view (funDef fn)

runMyopiaM :: MyopiaM a -> Program -> a
runMyopiaM f prog = fst $ evalRWS f (emptyEnv & program .~ prog) SM.empty

runMyopiaT :: (Functor m, Monad m) => MyopiaT m a -> Program -> m a
runMyopiaT f prog = fst <$> evalRWST f (emptyEnv & program .~ prog) SM.empty

runMyopiaT' :: (Functor m, Monad m) => BuiltinMap m -> MyopiaT m a -> Program -> m a
runMyopiaT' bm f prog = fst <$> evalRWST f (Env { _program = prog, _builtins = bm }) SM.empty

getArity :: Program -> FunName -> Integer
getArity prog fn = runMyopiaM (getDef fn >>= arityM) prog

eval :: (Functor m, Monad m) => Expr -> [Integer] -> MyopiaT m Integer
-- showing only the first param to prevent evaling bottom
eval e p | trace ("eval " ++ show e ++ " [" ++ show (head p) ++ ",…]" ) False = undefined
eval Z [_] = return 0
eval S [x] = return $ x + 1
eval (I i _) xs = return $ xs !! (i - 1)
eval (C h gs) xs = mapM (`eval` xs) gs >>= eval h
eval (P g _) (0:xs) = eval g xs
eval (P g h) ((pred -> y):xs) = do
    rec <- eval (P g h) (y:xs)
    eval h (y : rec : xs)
eval (M f) xs = minimize f xs 0
eval (FC "bottom") _xs = error "Function \"bottom\" called."
eval (FC fn) xs = do
    memoizedValue <- use $ at (fn, xs)
    case memoizedValue of
        Just value -> return value
        Nothing -> do
            value <- getDef fn >>= flip eval xs
            at (fn,xs) ?= value
            return value

runProgram :: Program -> FunName -> [Integer] -> Integer
runProgram prog fn params = runMyopiaM (getDef fn >>= flip eval params) prog

minimize :: (Functor m, Monad m) => Expr -> [Integer] -> Integer -> MyopiaT m Integer
minimize f xs z = do
    yz <- eval f (z:xs)
    if yz == 0 then return z else minimize f xs (z+1)

-- vim: set ts=4 sw=4 et
