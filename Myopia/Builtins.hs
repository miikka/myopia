module Myopia.Builtins where

import Data.Char (ord)
import qualified Data.Map   as M

import           Myopia.AST

emptyBuiltins :: (Monad m) => BuiltinMap m
emptyBuiltins = M.empty

pureBuiltins :: (Monad m) => BuiltinMap m
pureBuiltins = M.map (return .) $  M.fromList
    [("exp", \[x,y] -> x^y)
    ,("div", \[x,y] -> if x `mod` y == 0 then 1 else 0)
    ]

ioBuiltins :: IO (BuiltinMap IO)
ioBuiltins = do
    input <- getContents
    return $ M.fromList
        [("trace", \l@(x:_) -> print l >> return x)
        ,("ioChar", \[x] -> return . fi . ord $ input !! fi x)]
  where
    fi :: (Integral a, Num b) => a -> b
    fi = fromIntegral

-- vim: set ts=4 sw=4 et
