module BuiltinFunctions where

import qualified Data.Csv.Parser as CSV

import Workspace

biMax :: [SHData] -> SHData
biMax ((DataValue a):(DataValue b):_) = DataValue $ max a b

loadMatrix :: String -> IO (SHMatrix)
loadMatrix file = do
  contents <- readFile file
  CSV.csv CSV.defaultDecodeOptions
