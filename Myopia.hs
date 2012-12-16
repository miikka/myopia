{-# LANGUAGE DeriveDataTypeable #-}
import Control.Lens
import Control.Monad                   (forM, liftM)
import Data.Char                       (chr)
import Data.Monoid                     (mappend)
import System.Console.CmdArgs.Implicit
import Text.PrettyPrint.Leijen

import Myopia.AST
import Myopia.Builtins
import Myopia.Eval
import Myopia.Parser                   (parseFile)
import Myopia.Pretty
import Myopia.REPL
import Myopia.TypeCheck                (TypeError, typeCheck)

data Myopia = Run { file           :: FilePath
                  , function       :: FunName
                  , enableBuiltins :: Bool
                  , enableIO       :: Bool
                  }
            | Repl deriving (Show, Data, Typeable)

myopia = modes [run &= auto, repl]
         &= summary "Myopia v. 0.1.0.0 <https://github.com/miikka/myopia/>"
  where
    run = Run
          { file = def &= argPos 0 &= typ "FILE"
          , function = def &= argPos 1 &= typ "FUNCTION" &= opt "main"
          , enableBuiltins = False &= explicit &= name "builtins"
            &= help "Enable Haskell implementations of some functions."
          , enableIO = False &= explicit &= name "io"
            &= help "Enable IO mode."
          } &= details ["Evaluate a function from a file."]
    repl = Repl &= details ["Start an interactive Myopia session."]

main :: IO ()
main = do
    mode <- cmdArgs myopia
    case mode of
        Repl -> repl
        opts@(Run {}) -> runFile opts

printError :: TypeError -> IO ()
printError (ctx, e, msg) = do
    putStrLn $ "Context: " ++ ctx
    putStrLn $ "Message: " ++ msg
    putStrLn $ "Node:\n" ++ show (indent 4 $ pretty e)

runFile :: Myopia -> IO ()
runFile opts = do
    let name = function opts
    prog <- parseFile (file opts)
    case prog ^. funDefs.at name of
        Nothing -> error $ "No such function: " ++ name
        Just def -> do
            let mainArity = runMyopiaM (arityM def) prog
            case typeCheck prog of
                Left e   -> printError e
                Right () -> do
                    case enableIO opts of
                        True -> do
                            let bm = if enableBuiltins opts then pureBuiltins `mappend` ioBuiltins else emptyBuiltins
                            runIO opts prog bm 0
                        False -> do
                            let bm = if enableBuiltins opts then pureBuiltins else emptyBuiltins
                            putStrLn $ "Expecting " ++ show mainArity ++ " parameters."
                            params <- forM [1..mainArity] (\_ -> liftM read getLine)
                            print $ runProgram' bm prog name params

runIO :: Myopia -> Program -> BuiltinMap IO -> Integer -> IO ()
runIO opts prog bm n = do
    v <- runProgram'' bm prog (function opts) [n]
    case v of
        0 -> return ()
        v -> do
            putChar $ chr (fromIntegral v)
            runIO opts prog bm (succ n)

-- vim: set ts=4 sw=4 et
