module Myopia.TypeCheck where

import           Control.Applicative
import           Control.Lens
import           Control.Monad              (unless)
import           Control.Monad.RWS          (asks)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT (..), left)
import           Data.Map.Lazy              (Map)
import qualified Data.Map.Lazy              as M
import           Text.Printf                (printf)

import           Myopia.AST

type TypeError = (FunName, Expr, String)
type TypeCheckM = EitherT TypeError MyopiaM

ensure :: Bool -> String -> Expr -> String -> TypeCheckM ()
ensure p ctx e msg = unless p $ left (ctx, e, msg)

checkArity :: FunName -> Expr -> TypeCheckM ()
checkArity ctx e@(C h gs) = do
    checkArity ctx h
    mapM_ (checkArity ctx) gs
    a <- fromIntegral <$> lift (arityM h)
    res <- lift $ mapM arityM gs
    ensure (length gs == a) ctx e $ printf "Got %d inner functions, %d expected." (length gs) a
    ensure (all (== head res) res) ctx e $ printf "All inner functions should have equal arity. Got: %s" (show res)
checkArity ctx e@(P g h) = do
    checkArity ctx g
    checkArity ctx h
    ag <- lift $ arityM g
    ah <- lift $ arityM h
    ensure (ah == ag + 2) ctx e $ printf "Arity of case n+1 should be (2 + arity of case 0). Was: %d, %d." ag ah
checkArity ctx e@(M f) = do
    checkArity ctx f
    x <- lift $ arityM f
    ensure (x > 1) ctx e $ printf "Arity of minimized function should be at least 2. Got: %d" x
checkArity _ (FC n) = lift (getDef n) >>= checkArity n
checkArity _ _ = return ()

checkTypeDef :: FunName -> Expr -> TypeCheckM ()
checkTypeDef fn def = do
    typeDef_ <- lift $ view (typeDefs.at fn)
    case typeDef_ of
        Nothing -> return ()
        Just typeDef -> do
            arity <- lift $ arityM def
            ensure (arity == typeDef) fn def $ printf "Declared arity %d, inferred arity %d." typeDef arity

checkFunction :: FunName -> TypeCheckM ()
checkFunction fn = do
    def <- lift $ getDef fn
    checkArity   fn def
    checkTypeDef fn def

runTypeCheckM :: TypeCheckM a -> Program -> Either TypeError a
runTypeCheckM f = runMyopiaM (runEitherT f)

traverseKeys_ :: Applicative t => (k -> t ()) -> Map k a -> t ()
traverseKeys_ f m = M.traverseWithKey (\k _ -> f k *> pure ()) m *> pure ()

typeCheck :: Program -> Either TypeError ()
typeCheck = runTypeCheckM $ lift (view funDefs) >>= traverseKeys_ checkFunction

-- vim: set ts=4 sw=4 et
