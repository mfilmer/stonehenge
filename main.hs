module Tmp where

import qualified Data.Map.Strict as M

import Grammar
import Tokens

type Workspace = M.Map String Data
emptyWorkspace :: Workspace
emptyWorkspace = M.empty

data Data = DataInt Int
          | DataDouble Double

-- Needs +, -, *, abs, signum, fromInteger
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

-- Needs fromRational, /
instance Fractional Data where
  DataInt a / DataInt b = DataInt $ a `div` b
  DataInt a / DataDouble b = DataDouble $ fromIntegral a / b
  DataDouble a / DataInt b = DataDouble $ a / fromIntegral b
  DataDouble a / DataDouble b = DataDouble $ a / b
  fromRational a = DataDouble $ fromRational a

evalWksp :: Exp -> Workspace -> (Data, Workspace)
evalWksp (Assign name a) ws = (aVal, finalWs)
  where
    (aVal, ws1) = evalWksp a ws
    finalWs = M.insert name aVal ws1
evalWksp (Plus a b) ws = (aVal + bVal, finalWs)
  where
    (aVal, ws1) = evalWksp a ws
    (bVal, finalWs) = evalWksp b ws1
evalWksp (Minus a b) ws = (aVal - bVal, finalWs)
  where
    (aVal, ws1) = evalWksp a ws
    (bVal, finalWs) = evalWksp b ws1
evalWksp (Times a b) ws = (aVal * bVal, finalWs)
  where
    (aVal, ws1) = evalWksp a ws
    (bVal, finalWs) = evalWksp b ws1
evalWksp (Div a b) ws = (aVal / bVal, finalWs)
  where
    (aVal, ws1) = evalWksp a ws
    (bVal, finalWs) = evalWksp b ws1
evalWksp (Negate a) ws = (-aVal, finalWs)
  where
    (aVal, finalWs) = evalWksp a ws
evalWksp (Int a) ws = (DataInt a, ws)
evalWksp (Double a) ws = (DataDouble a, ws)
evalWksp (Var name) ws = (ws M.! name, ws)
