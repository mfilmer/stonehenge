module Main where

import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout, hIsEOF, stdin)
import System.Environment (getArgs)
import BuiltinFunctions

import Grammar
import Tokens
import Workspace

startingWorkspace :: M.Map String SHData
startingWorkspace = M.fromList [("max", DataFcn 2 biMax)]

evalExp :: Exp -> Workspace -> SHData
evalExp (Call name a) ws = fcn aVal
  where
    DataFcn nArgs fcn = ws M.! name
    aVal = map (\x -> evalExp x ws) a
evalExp (Plus a b) ws = DataValue (aVal + bVal)
  where
    DataValue aVal = evalExp a ws
    DataValue bVal = evalExp b ws
evalExp (Minus a b) ws = DataValue (aVal - bVal)
  where
    DataValue aVal= evalExp a ws
    DataValue bVal = evalExp b ws
evalExp (Times a b) ws = DataValue (aVal * bVal)
  where
    DataValue aVal = evalExp a ws
    DataValue bVal = evalExp b ws
evalExp (Div a b) ws = DataValue (aVal / bVal)
  where
    DataValue aVal= evalExp a ws
    DataValue bVal = evalExp b ws
evalExp (Negate a) ws = DataValue (-aVal)
  where
    DataValue aVal = evalExp a ws
evalExp (Int a) ws = DataValue (SHInt (fromIntegral a))
evalExp (Double a) ws = DataValue (SHDouble a)
evalExp (Var name) ws = ws M.! name


evalStmnts :: Workspace -> [Stmnt] -> IO (Workspace)
evalStmnts ws [] = return ws
evalStmnts ws ((Stmnt exp):xs) = do
  putStrLn $ show val
  evalStmnts ws xs
  where
    val = evalExp exp ws
evalStmnts ws ((StmntAssign name exp):xs) = do
  putStrLn $ show val
  evalStmnts ws1 xs
  where
    val = evalExp exp ws
    ws1 = M.insert name val ws

replLoop :: Workspace -> IO ()
replLoop ws = do
  putStr "> "
  hFlush stdout
  iseof <- hIsEOF stdin
  if iseof
    then return ()
    else do
      line <- getLine
      ws1 <- (evalStmnts ws) . reverse . expCalc $ alexScanTokens line
      replLoop ws1

runProgram :: Workspace -> [String] -> IO ()
runProgram _ [] = return ()
runProgram ws (file:files) = do
  contents <- readFile file
  let ast = expCalc (alexScanTokens contents)
  ws1 <- (evalStmnts ws) . reverse . expCalc $ alexScanTokens contents
  runProgram ws1 files

main :: IO ()
main = getArgs >>= parse
  where
    parse [] = replLoop startingWorkspace
    parse a = runProgram startingWorkspace a
  
