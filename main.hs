module Main where

import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout, hIsEOF, stdin)
import BuiltinFunctions

import Grammar
import Tokens
import Workspace

startingWorkspace :: M.Map String SHData
startingWorkspace = M.fromList [("max", DataFcn 2 biMax)]

{-
evalWksp :: Exp -> Workspace -> Maybe (Data, Workspace)
evalWksp (Assign name a) ws = undefined
evalWksp (Call name a) ws = undefined
evalWksp (Plus a b) ws = undefined
evalWksp (Minus a b) ws = undefined
evalWksp (Times a b) ws = undefined
evalWksp (Div a b) ws = undefined
evalWksp (Negate a) ws = undefined
evalWksp (Int a) ws = undefined
evalWksp (Double a) ws = undefined
evalWksp (Var name) ws = undefined
-}

evalWksp :: Exp -> Workspace -> (SHData, Workspace)
evalWksp (Assign name a) ws = (aVal, finalWs)
  where
    (aVal, ws1) = evalWksp a ws
    finalWs = M.insert name aVal ws1
evalWksp (Call name a) ws = (fcn aVal, ws)
  where
    DataFcn nArgs fcn = ws M.! name
    aVal = map fst $ map (\x -> evalWksp x ws) a
evalWksp (Plus a b) ws = (DataValue (aVal + bVal), finalWs)
  where
    (DataValue aVal, ws1) = evalWksp a ws
    (DataValue bVal, finalWs) = evalWksp b ws1
evalWksp (Minus a b) ws = (DataValue (aVal - bVal), finalWs)
  where
    (DataValue aVal, ws1) = evalWksp a ws
    (DataValue bVal, finalWs) = evalWksp b ws1
evalWksp (Times a b) ws = (DataValue (aVal * bVal), finalWs)
  where
    (DataValue aVal, ws1) = evalWksp a ws
    (DataValue bVal, finalWs) = evalWksp b ws1
evalWksp (Div a b) ws = (DataValue (aVal / bVal), finalWs)
  where
    (DataValue aVal, ws1) = evalWksp a ws
    (DataValue bVal, finalWs) = evalWksp b ws1
evalWksp (Negate a) ws = (DataValue (-aVal), finalWs)
  where
    (DataValue aVal, finalWs) = evalWksp a ws
evalWksp (Int a) ws = (DataValue (SHInt (fromIntegral a)), ws)
evalWksp (Double a) ws = (DataValue (SHDouble a), ws)
evalWksp (Var name) ws = (ws M.! name, ws)

mainLoop :: Workspace -> IO()
mainLoop ws = do
  putStr "> "
  hFlush stdout
  iseof <- hIsEOF stdin
  if iseof
    then return ()
    else do
      line <- getLine
      let ast = expCalc (alexScanTokens line)
      let (newData, newWs) = evalWksp ast ws
      print newData
      mainLoop newWs

main :: IO ()
main = do
  mainLoop startingWorkspace
  
