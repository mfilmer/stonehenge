module Workspace where

import qualified Data.Map.Strict as M
import Data.List (intersperse)

type Workspace = M.Map String Data
emptyWorkspace :: Workspace
emptyWorkspace = M.empty

data Data = DataInt Int
          | DataDouble Double
          | DataMatrix [[Data]]
          | DataTable [String] [[Data]]
          | DataFcn Int ([Data] -> Data)

isDataInt (DataInt _) = True
isDataInt _           = False

isDataDouble (DataDouble _) = True
isDataDouble _               = False

isDataMatrix (DataMatrix _) = True
isDataMatrix _              = False

isDataTable (DataTable _ _) = True
isDataTable _               = False

instance Eq Data where
  DataInt a == DataInt b = a == b
  DataInt a == DataDouble b = fromIntegral a == b
  DataDouble a == DataInt b = a == fromIntegral b
  DataDouble a == DataDouble b = a == b

instance Ord Data where
  DataInt a <= DataInt b = a <= b
  DataInt a <= DataDouble b = fromIntegral a <= b
  DataDouble a <= DataInt b = a <= fromIntegral b
  DataDouble a <= DataDouble b = a <= b

instance Show Data where
  show (DataInt a) = show a
  show (DataDouble a) = show a
  show (DataMatrix _) = ""
  show (DataTable _ _) = ""
  show (DataFcn nArgs _) = "Fcn(" ++ (intersperse ',' (take nArgs (cycle ['a'..'z']))) ++ ")"

instance Num Data where
  DataInt a + DataInt b = DataInt $ a + b
  DataInt a + DataDouble b = DataDouble $ fromIntegral a + b
  DataDouble a + DataInt b = DataDouble $ a + fromIntegral b
  DataDouble a + DataDouble b = DataDouble $ a + b
  DataInt a - DataInt b = DataInt $ a - b
  DataInt a - DataDouble b = DataDouble $ fromIntegral a - b
  DataDouble a - DataInt b = DataDouble $ a - fromIntegral b
  DataDouble a - DataDouble b = DataDouble $ a - b
  DataInt a * DataInt b = DataInt $ a * b
  DataInt a * DataDouble b = DataDouble $ fromIntegral a * b
  DataDouble a * DataInt b = DataDouble $ a * fromIntegral b
  DataDouble a * DataDouble b = DataDouble $ a * b
  abs (DataInt a) = DataInt $ abs a
  abs (DataDouble a) = DataDouble $ abs a
  signum (DataInt a) = DataInt $ signum a
  signum (DataDouble a) = DataDouble $ signum a
  fromInteger a = DataInt $ fromInteger a

instance Fractional Data where
  DataInt a / DataInt b = DataInt $ a `div` b
  DataInt a / DataDouble b = DataDouble $ fromIntegral a / b
  DataDouble a / DataInt b = DataDouble $ a / fromIntegral b
  DataDouble a / DataDouble b = DataDouble $ a / b
  fromRational a = DataDouble $ fromRational a
