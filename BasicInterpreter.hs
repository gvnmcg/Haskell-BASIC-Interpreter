{-----------------------------------------------------------------------------
 - 
 - BasicInterpreter.hs
 -
 - Christopher Salinas
 - Gavin McGuire
 -
 - CS 456
 - Fall 2020
 - University of New Mexico
 -
 -----------------------------------------------------------------------------}

module BasicInterpreter where
  import BasicData
  import Control.Monad
  import Control.Monad.IO.Class
  import Control.Monad.Trans.Reader
  import Data.Array.Base
  import Data.List
  import System.IO
  import System.Exit
  import System.Random
  import Util
  import Variables

  initArray :: Expr -> Interpreter ()
  initArray (Array' (Id var) es) = do
    dims <- mapM evalExpr es
    case dims of
      [j] -> dim1D var (truncate j)
      [i,j] -> dim2D var (truncate i) (truncate j)
      _ -> error "Incorrect array dimensions"

  unwrapVal :: Maybe Float -> Float
  unwrapVal m = case m of
      Just v -> v
      Nothing -> error "Failed to evaluate variable"

  setVar :: Expr -> Float -> Interpreter ()
  setVar (Id var) val = setVariable var val
  setVar (Array' (Id var) [ej]) val = do
    j <- evalExpr ej
    setVariable1D var (truncate j) val
  setVar (Array' (Id var) [ei,ej]) val = do
    i <- evalExpr ei
    j <- evalExpr ej
    setVariable2D var (truncate i) (truncate j) val

  -------------------- EVAL EXPRESSIONS --------------------
  evalExpr :: Expr -> Interpreter Float
  -- Id
  evalExpr i@(Id _) = do
    m <- evalVariable i
    return $ unwrapVal m

  -- Num
  evalExpr (Num n) = return n

  -- Int'
  evalExpr (Int' i) = do
    n <- evalExpr i
    return $ fromIntegral $ truncate n

  -- Rnd
  evalExpr (Rnd r) = do
    e <- evalExpr r
    n <- liftIO $ randomRIO (0, e)
    if e > 1 
      then return $ fromIntegral $ truncate n
      else return n

  -- MultExpr
  evalExpr (MultExpr '*' exprL exprR) = do
    l <- evalExpr exprL
    r <- evalExpr exprR
    return $ l * r

  evalExpr (MultExpr '/' exprL exprR) = do
    l <- evalExpr exprL
    r <- evalExpr exprR
    return $ l / r

  -- AddExpr
  evalExpr (AddExpr '+' exprL exprR) = do
    l <- evalExpr exprL
    r <- evalExpr exprR
    return $ l + r

  evalExpr (AddExpr '-' exprL exprR) = do
    l <- evalExpr exprL
    r <- evalExpr exprR
    return $ l - r
  
  -- NegateExpr
  evalExpr (NegateExpr expr) = do
    e <- evalExpr expr
    return (-e)

  -- PowerExpr
  evalExpr (PowerExpr exprB exprP) = do
    b <- evalExpr exprB
    p <- evalExpr exprP
    return $ b ^ truncate p

  -- Array'
  evalExpr a@(Array' _ _) = do
    m <- evalVariable a
    return $ unwrapVal m

  -- SepExpr
  evalExpr (SepExpr _ e) = evalExpr e

  -- Otherwise, eval comparison
  evalExpr e = do 
    b <- evalComparison e
    return $ if b then 1 else 0

  -------------------- EVAL VARIABLES --------------------
  evalVariable :: Expr -> Interpreter (Maybe Float)
  evalVariable (Id var) = getVariable var

  evalVariable (Array' (Id var) [ej]) = do
    j <- evalExpr ej
    getVariable1D var (truncate j)

  evalVariable (Array' (Id var) [ei,ej]) = do
    i <- evalExpr ei
    j <- evalExpr ej
    getVariable2D var (truncate i) (truncate j)

  -------------------- EVAL COMPARISONS --------------------
  evalComparison :: Expr -> Interpreter Bool
  -- OrExpr (written so it short circuits)
  evalComparison (OrExpr lhs rhs) = do
    l <- evalComparison lhs
    if l
      then return True
      else evalComparison rhs

  -- AndExpr (written so it short circuits)
  evalComparison (AndExpr lhs rhs) = do
    l <- evalComparison lhs
    if not l
      then return False
      else evalComparison rhs

  -- NotExpr
  evalComparison (NotExpr expr) = do
    e <- evalComparison expr
    return $ not e

  -- CompareExpr
  evalComparison (CompareExpr "=" lhs rhs) = do
    l <- evalExpr lhs
    r <- evalExpr rhs
    return $ l == r
 
  evalComparison (CompareExpr "<>" lhs rhs) = do
    l <- evalExpr lhs
    r <- evalExpr rhs
    return $ l /= r

  evalComparison (CompareExpr ">" lhs rhs) = do
    l <- evalExpr lhs
    r <- evalExpr rhs
    return $ l > r

  evalComparison (CompareExpr ">=" lhs rhs) = do
    l <- evalExpr lhs
    r <- evalExpr rhs
    return $ l >= r
    
  evalComparison (CompareExpr "<" lhs rhs) = do
    l <- evalExpr lhs
    r <- evalExpr rhs
    return $ l < r
 
  evalComparison (CompareExpr "<=" lhs rhs) = do
    l <- evalExpr lhs
    r <- evalExpr rhs
    return $ l <= r

  evalComparison _ = error "Expression evaluation not implemented"

  -------------------- EVAL LISTS --------------------
  -- evalPrintList takes a tuple of list of expressions and a boolean
  --   The boolean represents whether this PrintList is a new line expression
  --     A new line expression is on that does not contain a ';' or ','.
  evalPrintList :: [Expr] -> Interpreter String
  evalPrintList [] = return ""
  evalPrintList ((SepExpr _ e):es) = evalPrintList $ e:es
  evalPrintList ((String' s):es) = do
    ss <- evalPrintList es
    return $ s ++ ss
  evalPrintList (e:es) = do
    f <- evalExpr e
    ss <- evalPrintList es
    return $ showNum f ++ ss

  evalExprList :: [Expr] -> Interpreter [Float]
  evalExprList = mapM evalExpr

  -------------------- EVAL STATEMENT --------------------
  evalStatement :: Statement -> Interpreter (Bool, Bool)
  ---------- DIM ----------
  evalStatement (DIM []) = return (True, True)
  evalStatement (DIM (a:as)) = do
    initArray a
    evalStatement (DIM as)

  ---------- LET ----------
  evalStatement (LET var rhs) = do
    val <- evalExpr rhs
    setVar var val
    return (True, True)

  ---------- PRINT ----------
  -- Print New Line Statement
  evalStatement (PRINT []) = do
    liftIO $ putStrLn ""
    liftIO $ hFlush stdout
    return (True, True)

  -- Print Tab Statement
  evalStatement (PRINT ((SepExpr _ (Tab e)):es)) = do
    n <- evalExpr e
    let s = replicate (truncate n) ' '
    ss <- evalPrintList es
    liftIO $ putStrLn $ s ++ ss
    liftIO $ hFlush stdout
    return (True, True)
  
  -- Print SepExpr List
  evalStatement (PRINT ((SepExpr _ e):es)) = do
    s <- evalPrintList $ e:es
    liftIO $ putStr s
    return (True, True)

  -- Print List
  evalStatement (PRINT es) = do
    s <- evalPrintList es
    liftIO $ putStrLn s
    liftIO $ hFlush stdout
    return (True, True)

  ---------- FOR ----------
  -- If variable is Nothing, initialize it and handle it appropriately
  -- If variable is (Just _), handle appropriately
  evalStatement (FOR (Id var) init l ms) = do
    mv <- getVariable var
    limit <- evalExpr l
    step <- evalStep ms
    case mv of
      Nothing -> do
        val <- evalExpr init
        setVariable var val
        handleFor var val limit step
      Just val -> do handleFor var val limit step

  ---------- NEXT ----------
  -- For each Id in the list, check what the variable resolves to
  --   1. If Nothing, continue to next Id
  --   2. If Just val,
  --     a. Set var = val + step
  --     b. Check limit
  --       i. if met, continue
  --       ii. GOTO associate FOR statement
  evalStatement (NEXT []) = return (True, True)
  evalStatement (NEXT ((Id var):ids)) = do
    v <- getVariable var
    case v of
      Nothing -> evalStatement (NEXT ids)
      Just val -> do
        (Line ln [FOR (Id var) _ l s]) <- findFor var
        limit <- evalExpr l
        step <- evalStep s
        let newVal = val + step
        if metLimit newVal limit step
          then do
            clearVariable var
            evalStatement (NEXT ids)
            return (True, True)
          else do
            setVariable var newVal
            evalStatement $ GOTO $ Num $ fromIntegral ln

  ---------- IFTHEN ----------
  evalStatement (IFTHEN e ln) = do
    b <- evalComparison e
    if b 
      then evalStatement $ GOTO ln
      else return (True, True)

  ---------- INPUT ----------
  evalStatement (INPUT str vars) = do
    when (str /= "") $ do liftIO $ putStrLn str
    inputLines vars
    return (True, True)

  ---------- GOTO ----------
  evalStatement (GOTO ln) = do
    nextLine <- findLine $ truncate (val ln)
    setLineIx nextLine
    return (False, True)

  ---------- GOSUB ----------
  evalStatement (GOSUB ln) = do
    (program, vars, _, arrs) <- ask
    nextLine <- findLine $ truncate (val ln)
    newLineArr <- liftIO $ newArray (1, 1) nextLine
    liftIO $ runReaderT evalProgram (program, vars, newLineArr, arrs)
    return (True, True)

  ---------- ONGOTO ----------
  evalStatement (ONGOTO e lns) = do
    ix <- evalExpr e
    ln <- evalExpr $ lns !! (truncate ix - 1)
    evalStatement $ GOTO (Num ln)

  ---------- RETURN ----------
  evalStatement RETURN = return (False, False)

  ---------- REM ----------
  evalStatement (REM _) = return (True, True)

  ---------- END ----------
  evalStatement END = liftIO exitSuccess

  evalStatements :: [Statement] -> Interpreter Bool
  evalStatements [] = return True
  evalStatements (s:ss) = do
    (continueToNextStatement, continueToNextLine) <- evalStatement s
    if continueToNextStatement
      then evalStatements ss
      else return continueToNextLine

  evalProgram :: Interpreter ()
  evalProgram = do
    (program, _, _, _) <- ask
    lineNumber <- getLineIx
    when (lineNumber <= length program) $ do
      let (Line _ ss) = program !! lineNumber
      setLineIx (lineNumber + 1)
      continue <- evalStatements ss
      when continue evalProgram

  interpret :: Program -> IO ()
  interpret program = do
    vars <- newArray ('A','Z') Nothing
    arrs <- newArray ('A','Z') Nothing
    currentLineArr <- newArray (1, 1) 0
    runReaderT evalProgram (program, vars, currentLineArr, arrs)

  -------------------- FOR HELPERS --------------------
  evalStep :: Maybe Expr -> Interpreter Float
  evalStep ms = case ms of
    Nothing -> return 1
    Just step -> do evalExpr step

  metLimit :: Float -> Float -> Float ->  Bool
  metLimit val limit step = if step < 0
    then val < limit
    else val > limit

  -- Check limit
  --   a. If not met, continue
  --   b. If met,
  --     i. clear variable
  --     ii. GOTO associated NEXT statement
  handleFor :: Char -> Float -> Float -> Float -> Interpreter (Bool, Bool)
  handleFor var val limit step = do
    if not $ metLimit val limit step
      then return (True, True)
      else do
        clearVariable var
        ln <- findNext var
        exitFor ln

  exitFor :: Int -> Interpreter (Bool, Bool)
  exitFor lineNumOfNEXT = do
    ln <- getLineIx
    let cIx = ln - 1
    nIx <- findLine lineNumOfNEXT
    if cIx == nIx
      then return (False, True)
      else do
        setLineIx nIx
        return (False, True)

  -------------------- INPUT HELPERS --------------------
  inputLines :: [Expr] -> Interpreter ()
  inputLines [] = return ()
  inputLines ((Id var):lns) = do
    inp <- liftIO getLine
    setVariable var (read inp)
    inputLines lns
