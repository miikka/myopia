import           Control.Applicative
import           Control.Monad
import qualified Data.Map.Lazy       as M
import           Text.PrettyPrint.Leijen
import           System.Environment

import           Myopia.AST
import           Myopia.Parser       (parseFile)
import           Myopia.Pretty
import           Myopia.REPL
import           Myopia.TypeCheck


main :: IO ()
main = do
    (fp : name : _) <- getArgs
    if fp == "repl" then repl else runFile fp name

printError :: TypeError -> IO ()
printError (ctx, e, msg) = do
    putStrLn $ "Context: " ++ ctx
    putStrLn $ "Message: " ++ msg
    putStrLn $ "Node:\n" ++ show (indent 4 $ pretty e)

runFile :: FilePath -> String -> IO ()
runFile fp name = do
    prog <- parseFile fp
    case M.lookup name (funDefs prog) of
        Nothing -> error $ "No such function: " ++ name
        Just def -> do
            let mainArity = runMyopiaM (arityM def) prog
            case typeCheck prog of
                Left e   -> printError e
                Right () -> do
                    putStrLn $ "Expecting " ++ show mainArity ++ " parameters."
                    params <- forM [1..mainArity] (\_ -> liftM read getLine)
                    print $ runProgram prog name params


-- vim: set ts=4 sw=4 et
