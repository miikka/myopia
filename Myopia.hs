import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.Map.Lazy              (Map)
import qualified Data.Map.Lazy              as M
import           System.Environment
import           Text.Printf

import           Myopia.AST
import           Myopia.Parser              (parseFile)
import           Myopia.REPL

type TypeError = (FunName, Expr, String)

ensure :: Bool -> String -> Expr -> String -> EitherT TypeError MyopiaM ()
ensure p ctx e msg = unless p $ left (ctx, e, msg)

checkM :: FunName -> Expr -> EitherT TypeError MyopiaM ()
checkM ctx e@(C h gs) = do
    checkM ctx h
    mapM_ (checkM ctx) gs
    a <- fromIntegral <$> lift (arityM h)
    res <- lift $ mapM arityM gs
    ensure (length gs == a) ctx e $ printf "Got %d inner functions, %d expected." (length gs) a
    ensure (all (== head res) res) ctx e $ printf "All inner functions should have equal arity. Got: %s" (show res)
checkM ctx e@(P g h) = do
    checkM ctx g
    checkM ctx h
    ag <- lift $ arityM g
    ah <- lift $ arityM h
    ensure (ah == ag + 2) ctx e $ printf "Arity of case n+1 should be (2 + arity of case 0). Was: %d, %d." ag ah
checkM ctx e@(M f) = do
    checkM ctx f
    x <- lift $ arityM f
    ensure (x > 1) ctx e $ printf "Arity of minimized function should be at least 2. Got: %d" x
checkM _ (FC n) = lift (getDef n) >>= checkM n
checkM _ _ = return ()


main :: IO ()
main = do
    (fp : name : _) <- getArgs
    if fp == "repl" then repl else runFile fp name

traverseWithKey_ :: Applicative t => (k -> a -> t ()) -> Map k a -> t ()
traverseWithKey_ f m = M.traverseWithKey (\k a -> f k a *> pure a) m *> pure ()

printError :: TypeError -> IO ()
printError (ctx, e, msg) = do
    putStrLn $ "Context: " ++ ctx
    putStrLn $ "Node:    " ++ show e
    putStrLn $ "Message: " ++ msg

runFile :: FilePath -> String -> IO ()
runFile fp name = do
    prog <- parseFile fp
    let def = funDefs prog M.! name
        mainArity = runMyopiaM (arityM def) prog
    case runMyopiaM (runEitherT (checkM name def)) prog of
        Left e   -> printError e
        Right () -> do
            traverseWithKey_ (checkTypeDef prog) (typeDefs prog)
            putStrLn $ "Expecting " ++ show mainArity ++ " parameters."
            params <- forM [1..mainArity] (\_ -> liftM read getLine)
            print $ runProgram prog name params

checkTypeDef :: Program -> FunName -> Arity -> IO ()
checkTypeDef prog fn a =
    unless (runMyopiaM (arityM (funDefs prog M.! fn)) prog == a) $ error "Typechecking failed."

-- vim: set ts=4 sw=4 et
