{-# LANGUAGE ViewPatterns #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as M
import Debug.Trace
import System.Environment

import KTP.AST
import KTP.Parser (parseFile)

type KTPM = Reader Program

arity :: Expr -> Integer
arity Z = 1
arity S = 1
arity (I _ k) = k
arity (C _ gs) = arity (head gs)
arity (P g _) = arity g + 1
arity (M f) = arity f - 1
arity (FC _fn) = undefined

arityM :: Expr -> KTPM Integer
arityM Z = return 1
arityM S = return 1
arityM (I _ k) = return k
arityM (C _ gs) = arityM (head gs)
arityM (P g _) = succ <$> arityM g
arityM (M f) = pred <$> arityM f
arityM (FC fn) = getDef fn >>= arityM 

getDef :: FunName -> KTPM Expr
getDef fn = asks (M.! fn)

check :: Expr -> Bool
check Z = True
check S = True
check (I _ _) = True
check (C h gs) = length gs == fromIntegral (arity h) && let ars = map arity gs in all (== head ars) ars
check (P g h) = arity h == arity g + 2
check (M f) = arity f > 1
check (FC _) = True

checkM :: Expr -> KTPM Bool
checkM (C h gs) = do
    a <- arityM h
    res <- mapM arityM gs
    return $ length gs == fromIntegral a && all (== head res) res
checkM (P g h) = do
    ag <- arityM g
    ah <- arityM h
    return $ ah == ag + 2
checkM (M f) = arityM f >>= \x -> return (x > 1)
checkM _ = return True


foo :: KTPM [Integer]
foo = do
    a <- return undefined
    return [1,a,2]

eval :: Expr -> [Integer] -> KTPM Integer
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

runKTPM :: KTPM a -> Program -> a
runKTPM = runReader

runProgram :: Program -> FunName -> [Integer] -> Integer
runProgram prog fn params = runKTPM (eval (prog M.! fn) params) prog

minimize :: Expr -> [Integer] -> Integer -> KTPM Integer
minimize f xs z = do
    yz <- eval f (z:xs)
    if yz == 0 then return z else minimize f xs (z+1)

aPlusB :: Expr
aPlusB = P (I 1 1) (C S [I 2 3])

main :: IO ()
main = do
    (fp : name : _) <- getArgs
    prog <- parseFile fp
    print prog
    let def = prog M.! name
        mainArity = runKTPM (arityM def) prog
    unless (runKTPM (checkM def) prog) $ error "Typechecking failed."
    putStrLn $ "Expecting " ++ show mainArity ++ " parameters."
    params <- forM [1..mainArity] (\_ -> liftM read getLine)
    print $ runProgram prog name params

-- vim: set ts=4 sw=4
