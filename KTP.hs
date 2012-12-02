import           Control.Applicative
import           Control.Monad
import qualified Data.Map            as M
import           Debug.Trace
import           System.Environment

import           KTP.AST
import           KTP.Parser          (parseFile)
import           KTP.REPL

arity :: Expr -> Integer
arity Z = 1
arity S = 1
arity (I _ k) = k
arity (C _ gs) = arity (head gs)
arity (P g _) = arity g + 1
arity (M f) = arity f - 1
arity (FC _fn) = undefined

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

main :: IO ()
main = do
    (fp : name : _) <- getArgs
    if fp == "repl" then repl else runFile fp name

runFile :: FilePath -> String -> IO ()
runFile fp name = do
    prog <- parseFile fp
    print prog
    let def = prog M.! name
        mainArity = runKTPM (arityM def) prog
    unless (runKTPM (checkM def) prog) $ error "Typechecking failed."
    putStrLn $ "Expecting " ++ show mainArity ++ " parameters."
    params <- forM [1..mainArity] (\_ -> liftM read getLine)
    print $ runProgram prog name params

-- vim: set ts=4 sw=4
