{-----------------------------------------------------------------------------
 - 
 - Util.hs
 -
 - Christopher Salinas
 - Gavin McGuire
 -
 - CS 456
 - Fall 2020
 - University of New Mexico
 -
 -----------------------------------------------------------------------------}

module Util where
  import BasicData
  import Control.Monad.IO.Class
  import Control.Monad.Trans.Reader
  import Data.Array.Base

  -------------------- CURRENT LINE --------------------
  getLineIx :: Interpreter Int
  getLineIx = do
    (_, _, lineArr, _) <- ask
    liftIO $ readArray lineArr 1

  setLineIx :: Int -> Interpreter ()
  setLineIx ln = do
    (_, _, lineArr, _) <- ask
    liftIO $ writeArray lineArr 1 ln

  -------------------- GOTO HELPERS --------------------
  findLineLoop :: Int -> Int -> Interpreter (Maybe Int)
  findLineLoop ix lineNumber = do
    (program, _, _, _) <- ask
    if ix > length program
      then return Nothing
      else do
        let (Line l _) = program !! ix
        if l == lineNumber
          then return $ Just ix
          else findLineLoop (ix + 1) lineNumber

  findLine :: Int -> Interpreter Int
  findLine lineNumber = do
    mi <- findLineLoop 0 lineNumber
    case mi of
      Nothing -> error $ "Could not find line: " ++ show lineNumber
      Just ix -> return ix

  -------------------- FOR HELPERS --------------------
  findFor :: Char -> Interpreter BasicLine
  findFor c = do
    ln <- getLineIx
    m <- findForInProgram c ln
    case m of
      Just (ln, s) -> return $ Line ln [s]
      Nothing -> error $ "Could not find FOR with id: " ++ [c]

  findForInProgram :: Char -> Int -> Interpreter (Maybe (Int, Statement))
  findForInProgram c ix = do
    (program, _, _, _) <- ask
    if ix < 0
      then return Nothing
      else do
        let (Line ln stmts) = program !! ix
        case findForInStmts stmts c of
          Just s -> return $ Just (ln, s)
          Nothing -> findForInProgram c (ix - 1)

  findForInStmts :: [Statement] -> Char -> Maybe Statement
  findForInStmts [] _ = Nothing
  findForInStmts (s@(FOR (Id id) _ _ _):ss) c = if id == c
    then Just s
    else findForInStmts ss c
  findForInStmts (_:ss) c = findForInStmts ss c

  -------------------- NEXT HELPERS --------------------
  findNext :: Char -> Interpreter Int
  findNext c = do
    ln <- getLineIx
    mn <- findNextInProgram c (ln - 1)
    case mn of
      Just n -> return n
      Nothing -> error $ "Could not find NEXT with id: " ++ [c]

  findNextInProgram :: Char -> Int -> Interpreter (Maybe Int)
  findNextInProgram c ix = do
    (program, _, _, _) <- ask
    if ix >= length program
      then return Nothing
      else do
        let (Line ln stmts) = program !! ix
        if findNextInStmts stmts c
          then return $ Just ln
          else findNextInProgram c (ix + 1)

  findNextInStmts :: [Statement] -> Char -> Bool
  findNextInStmts [] _ = False
  findNextInStmts ((NEXT ids):ss) c = idInIds ids c || findNextInStmts ss c
  findNextInStmts (_:ss) c = findNextInStmts ss c

  idInIds :: [Expr] -> Char -> Bool
  idInIds [] _ = False
  idInIds ((Id id):ids) c = (c == id) || idInIds ids c
