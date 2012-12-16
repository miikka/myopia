{-# LANGUAGE ViewPatterns #-}
module Myopia.Eval where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans   (lift)
import Data.Functor.Identity (Identity (..))

import Debug.Trace

import Myopia.AST

eval :: (Functor m, Monad m) => Expr -> [Integer] -> MyopiaT m Integer
{-
-- showing only the first param to prevent evaling bottom
eval e p | trace ("eval " ++ show e ++ " [" ++ show (head p) ++ ",…]" ) False = undefined
-}
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
            bf_ <- view $ builtin fn
            case bf_ of
                Just bf -> lift $ bf xs
                Nothing -> do
                    value <- getDef fn >>= flip eval xs
                    at (fn,xs) ?= value
                    return value

runProgram :: Program -> FunName -> [Integer] -> Integer
runProgram prog fn params = runMyopiaM (getDef fn >>= flip eval params) prog

runProgram' :: BuiltinMap Identity -> Program -> FunName -> [Integer] -> Integer
runProgram' bm prog fn params = runIdentity $ runMyopiaT' bm (getDef fn >>= flip eval params) prog

runProgram'' :: (Functor m, Monad m) => BuiltinMap m -> Program -> FunName -> [Integer] -> m Integer
runProgram'' bm prog fn params = runMyopiaT' bm (getDef fn >>= flip eval params) prog

minimize :: (Functor m, Monad m) => Expr -> [Integer] -> Integer -> MyopiaT m Integer
minimize f xs z = do
    yz <- eval f (z:xs)
    if yz == 0 then return z else minimize f xs (z+1)

-- vim: set ts=4 sw=4 et
