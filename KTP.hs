import           Control.Applicative
import           Control.Monad
import           Data.Map.Lazy       (Map)
import qualified Data.Map.Lazy       as M
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

    hc <- checkM h
    gsc <- mapM checkM gs

    return $ length gs == fromIntegral a && all (== head res) res && and gsc && hc
checkM (P g h) = do
    ag <- arityM g
    ah <- arityM h
    gc <- checkM g
    hc <- checkM h
    return $ ah == ag + 2 && gc && hc
checkM (M f) = arityM f >>= \x -> checkM f >>= \fc -> return (x > 1 && fc)
checkM _ = return True

main :: IO ()
main = do
    (fp : name : _) <- getArgs
    if fp == "repl" then repl else runFile fp name

traverseWithKey_ :: Applicative t => (k -> a -> t ()) -> Map k a -> t ()
traverseWithKey_ f m = M.traverseWithKey (\k a -> f k a *> pure a) m *> pure ()

runFile :: FilePath -> String -> IO ()
runFile fp name = do
    prog <- parseFile fp
    print prog
    let def = funDefs prog M.! name
        mainArity = runKTPM (arityM def) prog
    unless (runKTPM (checkM def) prog) $ error "Typechecking failed."
    traverseWithKey_ (checkTypeDef prog) (typeDefs prog)
    putStrLn $ "Expecting " ++ show mainArity ++ " parameters."
    params <- forM [1..mainArity] (\_ -> liftM read getLine)
    print $ runProgram prog name params

checkTypeDef :: Program -> FunName -> Arity -> IO ()
checkTypeDef prog fn a =
    unless (runKTPM (arityM (funDefs prog M.! fn)) prog == a) $ error "Typechecking failed."

-- vim: set ts=4 sw=4 et
