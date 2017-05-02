module Workspace where

import qualified Data.Map.Strict as M
import qualified Data.Array.IArray as A
import Text.Printf (printf)
import Data.List (intersperse)

type Workspace = M.Map String SHData
emptyWorkspace :: Workspace
emptyWorkspace = M.empty

data SHData = DataValue SHValue
            | DataArray SHArray
            | DataMatrix SHMatrix
            | DataTable SHTable
            | DataFcn Int ([SHData] -> SHData)

data SHValue = SHInt Integer
             | SHDouble Double

data SHArray = SHArray (A.Array Integer SHData)
data SHMatrix = SHMatrix (A.Array (Int, Int) SHValue)
data SHColumn = SHColumn String (A.Array Integer SHValue)
data SHTable = SHTable [SHColumn]

buildSampleArray xs = SHArray $ A.array (1,fromIntegral (length xs)) [(i, x) | (i,x) <- zip [1..] xs]
sampleArray1D = buildSampleArray [DataValue (SHInt (2*i)) | i <- [1..5]]
sampleArray2D = buildSampleArray [DataArray sampleArray1D | _ <- [1..5]]
sampleArray3D = buildSampleArray [DataArray sampleArray2D | _ <- [1..2]]
sampleArray4D = buildSampleArray [DataArray sampleArray3D | _ <- [1..5]]

sampleMatrix = SHMatrix $ A.array ((1,1),(5,5)) [((x,y), SHInt (fromIntegral (x+y))) | x <- [1..5], y <- [1..5]]

sampleColumnInt = SHColumn "Int Col" (A.array (1, 10) [(i,fromIntegral i) | i <- [1..10]])
sampleColumnDouble = SHColumn "Double Col" (A.array (1, 10) [(i,SHDouble (fromIntegral i*0.1)) | i <- [1..10]])

instance Show SHData where
  show (DataValue a) = show a
  show (DataArray a) = show a
  show (DataMatrix a) = show a
  show (DataTable a) = show a
  show (DataFcn nArgs _) = "Fcn(" ++ (intersperse ',' (take nArgs (cycle ['a'..'z']))) ++ ")"

instance Eq SHValue where
  SHInt a == SHInt b = a == b
  SHInt a == SHDouble b = fromInteger a == b
  SHDouble a == SHInt b = a == fromInteger b
  SHDouble a == SHDouble b = a == b

instance Ord SHValue where
  SHInt a <= SHInt b = a <= b
  SHInt a <= SHDouble b = fromInteger a <= b
  SHDouble a <= SHInt b = a <= fromInteger b
  SHDouble a <= SHDouble b  = a <= b

instance Show SHValue where
  show (SHInt a) = show a
  show (SHDouble a) = show a

instance Show SHArray where
  show (SHArray arr)
    | nLevels arr <= 1 = "[" ++ (foldr1 (\x y -> x ++ ", " ++ y) (fmap show arr)) ++ "]"
    | nLevels arr == 2 = "[\n" ++ insertTabs ((foldr1 (\x y -> x ++ ",\n" ++ y) (fmap show arr))) ++ "\n]"
    | nLevels arr >= 3 = "[\n" ++ insertTabs ((foldr1 (\x y -> x ++ ",\n\n\n" ++ y) (fmap show arr))) ++ "\n]"
    where
      insertTabs :: String -> String
      insertTabs str = foldr1 (\x y -> x ++ "\n" ++ y) $ map ("  " ++) (lines str)
      nLevels :: (A.Array Integer SHData) -> Int
      nLevels a
            | length (A.indices a) == 0 = 1
            | otherwise = case (a A.! 1) of 
                            (DataValue _) -> 1
                            (DataFcn _ _) -> 1
                            (DataArray (SHArray da)) -> 1 + nLevels da
                            (DataMatrix _) -> 2
                            (DataTable _) -> 2

colShow :: Int -> SHValue -> String
colShow n (SHInt a) = printf ("%" ++ show n ++ "d") a
colShow n (SHDouble a) = printf ("%" ++ show n ++ ".3e") a

instance Show SHMatrix where
  show (SHMatrix arr) =
    "[" ++ 
    concat (intersperse ";\n " [concat (intersperse  ", " [colShow 6 (arr A.! (r,c)) | c <- [cmin..cmax]]) | r <- [rmin..rmax]])
    ++ "]"
    where
      ((rmin,cmin),(rmax,cmax)) = A.bounds arr

instance Show SHColumn where
  show (SHColumn label arr)
    | n == 0 = ""
    | otherwise = concat $ intersperse "\n" (label:sizedStrs)
    where
      (_,n) = A.bounds arr
      labelLen = length label
      sizedStrs = case (arr A.! 1) of
                    (SHDouble a) -> map (colShow (max 8 labelLen)) (A.elems arr)
                    (SHInt a) -> map (printf ("%" ++ show (max maxWidth labelLen) ++ "s")) rawStrs
                      where
                        maxWidth = maximum (map length rawStrs)
                        rawStrs :: [String]
                        rawStrs = map show (A.elems arr)
                    

instance Show SHTable where
  show a = ""

instance Num SHValue where
  SHInt a + SHInt b = SHInt $ a + b
  SHInt a + SHDouble b = SHDouble $ fromIntegral a + b
  SHDouble a + SHInt b = SHDouble $ a + fromIntegral b
  SHDouble a + SHDouble b = SHDouble $ a + b
  SHInt a - SHInt b = SHInt $ a - b
  SHInt a - SHDouble b = SHDouble $ fromIntegral a - b
  SHDouble a - SHInt b = SHDouble $ a - fromIntegral b
  SHDouble a - SHDouble b = SHDouble $ a - b
  SHInt a * SHInt b = SHInt $ a * b
  SHInt a * SHDouble b = SHDouble $ fromIntegral a * b
  SHDouble a * SHInt b = SHDouble $ a * fromIntegral b
  SHDouble a * SHDouble b = SHDouble $ a * b
  abs (SHInt a) = SHInt $ abs a
  abs (SHDouble a) = SHDouble $ abs a
  signum (SHInt a) = SHInt $ signum a
  signum (SHDouble a) = SHDouble $ signum a
  fromInteger a = SHInt $ fromInteger a
  

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
