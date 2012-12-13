{-# LANGUAGE ViewPatterns #-}
module Myopia.AST where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Monoid

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

data Program = Program
    { funDefs  :: Map FunName Fun
    , typeDefs :: Map FunName Arity
    } deriving (Show)

instance Monoid Program where
    mempty = Program mempty mempty
    mappend (Program a1 b1) (Program a2 b2) = Program (a1 <> a2) (b1 <> b2)

type MyopiaM = Reader Program

arityM :: Expr -> MyopiaM Integer
arityM Z = return 1
arityM S = return 1
arityM (I _ k) = return k
arityM (C _ gs) = arityM (head gs)
arityM (P g _) = succ <$> arityM g
arityM (M f) = pred <$> arityM f
arityM (FC fn) = getDef fn >>= arityM

getDef :: FunName -> MyopiaM Expr
getDef fn = asks ((M.! fn) . funDefs)

runMyopiaM :: MyopiaM a -> Program -> a
runMyopiaM = runReader

getArity :: Program -> FunName -> Integer
getArity prog fn = runMyopiaM (getDef fn >>= arityM) prog

eval :: Expr -> [Integer] -> MyopiaM Integer
-- showing only the first param to prevent evaling bottom
eval e p | trace ("eval " ++ show e ++ " [" ++ show (head p) ++ ",…]" ) False = undefined
eval Z [_] = return 0
eval S [x] = return $ x + 1
eval (I i _) xs = return $ xs !! (i - 1)
eval (C h gs) xs = do --eval h (mapM (`eval` xs) gs)
    args <- mapM (`eval` xs) gs
    eval h args
eval (P g _) (0:xs) = eval g xs
eval (P g h) ((pred -> y):xs) = do
    rec <- eval (P g h) (y:xs)
    eval h (y : rec :xs)
eval (M f) xs = minimize f xs 0
eval (FC "bottom") xs = error "Function \"bottom\" called."
eval (FC fn) xs = getDef fn >>= \f -> eval f xs

runProgram :: Program -> FunName -> [Integer] -> Integer
runProgram prog fn params = runMyopiaM (eval (funDefs prog M.! fn) params) prog

minimize :: Expr -> [Integer] -> Integer -> MyopiaM Integer
minimize f xs z = do
    yz <- eval f (z:xs)
    if yz == 0 then return z else minimize f xs (z+1)
