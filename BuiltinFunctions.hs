module BuiltinFunctions where

import Workspace

biMax :: [Data] -> Data
biMax (a:b:_) = max a b
