module Myopia.Builtins where

import qualified Data.Map   as M

import           Myopia.AST

emptyBuiltins :: (Monad m) => BuiltinMap m
emptyBuiltins = M.empty

pureBuiltins :: (Monad m) => BuiltinMap m
pureBuiltins = M.map (return .) $  M.fromList
    [("exp", \[x,y] -> x^y)
    ,("div", \[x,y] -> if x `mod` y == 0 then 1 else 0)
    ]

ioBuiltins :: BuiltinMap IO
ioBuiltins = M.fromList
    [("trace", \l@(x:_) -> print l >> return x)]

-- vim: set ts=4 sw=4 et
