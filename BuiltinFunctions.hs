module BuiltinFunctions where

import Workspace

biMax :: [SHData] -> SHData
biMax ((DataValue a):(DataValue b):_) = DataValue $ max a b
