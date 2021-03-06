module Myopia.REPL where

import           Control.Applicative
import           Control.Lens
import           Control.Monad            (forM)
import           Control.Monad.IO.Class
import qualified Data.Map                 as M
import           Data.Monoid              (mempty)
import           System.Console.Haskeline
import           Text.Parsec              (parse)

import           Myopia.AST
import           Myopia.Eval
import           Myopia.Parser

repl :: IO ()
repl = runInputT defaultSettings (loop $ mempty)
  where
    loop :: Program -> InputT IO ()
    loop prog = do
        minput <- getInputLine "µ> "
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> do
                let (cmd:args) = words input
                prog' <- runCommand cmd args prog
                loop prog'

runCommand :: String -> [String] -> Program -> InputT IO Program
runCommand "d" args prog =
    case parse def "<repl>" (unwords args) of
        Left err -> liftIO (print err) >> return prog
        Right (FunDef n d) -> return $ prog & funDefs . at n ?~ d
runCommand "e" [name] prog = do
    let arity = getArity prog name
    liftIO $ putStrLn $ "Expecting " ++ show arity ++ " parameters."
    params <- forM [1..arity] (\_ -> read <$> liftIO getLine)
    liftIO $ print $ runProgram prog name params
    return prog
runCommand _ _ prog = liftIO (putStrLn "Unknown command") >> return prog

-- vim: ts=4 sw=4
