module Workspace where

import qualified Data.Map.Strict as M
import qualified Data.Array.IArray as A
import Data.List (intersperse)

type Workspace = M.Map String Data
emptyWorkspace :: Workspace
emptyWorkspace = M.empty

data Data = DataValue DataValue
          | DataCollection DataCollection
          | DataFcn Int ([Data] -> Data)

data DataValue = DataInt Integer
               | DataDouble Double

data DataCollection = DataArray (A.Array Integer Data)
                    | DataMatrix (A.Array (Int, Int) DataValue)
                    | DataTable [String] (A.Array (Int, Int) DataValue)

buildSampleArray xs = DataArray $ A.array (1,fromIntegral (length xs)) [(i, x) | (i,x) <- zip [1..] xs]
sampleArray1D = buildSampleArray [DataValue (DataInt (2*i)) | i <- [1..5]]
sampleArray2D = buildSampleArray [DataCollection sampleArray1D | i <- [1..5]]


instance Eq Data where
  DataValue a == DataValue b = a == b
  _ == _ = False

-- It might not make sense to have Data an instance of Ord
instance Ord Data where
  DataValue a < DataValue b = a < b
  DataCollection _ < DataCollection _ = False
  DataFcn _ _ < DataFcn _ _ = False
  DataValue a <= DataValue b = a <= b
  DataCollection _ <= DataCollection _ = False
  DataFcn _ _ <= DataFcn _ _ = False
  DataValue a > DataValue b = a > b
  DataCollection _ > DataCollection _ = False
  DataFcn _ _ > DataFcn _ _ = False
  DataValue a >= DataValue b = a >= b
  DataCollection _ >= DataCollection _ = False
  DataFcn _ _ >= DataFcn _ _ = False
  max (DataValue a) (DataValue b) = DataValue $ max a b
  max (DataCollection a) (DataCollection _) = DataCollection a
  max (DataFcn a b)(DataFcn _ _) = DataFcn a b
  min (DataValue a) (DataValue b) = DataValue $ max a b
  min (DataCollection a)(DataCollection _) = DataCollection a
  min (DataFcn a b)(DataFcn _ _) = DataFcn a b

instance Show Data where
  show (DataValue a) = show a
  show (DataCollection a) = show a
  show (DataFcn nArgs _) = "Fcn(" ++ (intersperse ',' (take nArgs (cycle ['a'..'z']))) ++ ")"

instance Eq DataValue where
  DataInt a == DataInt b = a == b
  DataInt a == DataDouble b = fromInteger a == b
  DataDouble a == DataInt b = a == fromInteger b
  DataDouble a == DataDouble b = a == b

instance Ord DataValue where
  DataInt a <= DataInt b = a <= b
  DataInt a <= DataDouble b = fromInteger a <= b
  DataDouble a <= DataInt b = a <= fromInteger b
  DataDouble a <= DataDouble b  = a <= b

instance Show DataValue where
  show (DataInt a) = show a
  show (DataDouble a) = show a

instance Show DataCollection where
  show (DataArray arr)
    | nLevels arr <= 1 = "[" ++ (foldr1 (\x y -> x ++ ", " ++ y) (fmap show arr)) ++ "]"
    | nLevels arr == 2 = "[\n  " ++ (foldr1 (\x y -> x ++ ",\n  " ++ y) (fmap show arr)) ++ "\n]"
    | nLevels arr >= 3 = "3"
    where nLevels a
            | length (A.indices a) == 0 = 1
            | otherwise = case (a A.! 1) of 
                            (DataValue _) -> 1
                            (DataFcn _ _) -> 1
                            (DataCollection c) -> case c of 
                                                    (DataArray da) -> 1 + nLevels da
                                                    (DataMatrix _) -> 2
                                                    (DataTable _ _) -> 2
  

{-
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
-}
