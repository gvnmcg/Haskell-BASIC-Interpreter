{------------------------------------------------------------------------------
 - 
 - BasicData.hs
 -
 - Christopher Salinas
 - Gavin McGuire
 -
 - CS 456
 - Fall 2020
 - University of New Mexico
 -
------------------------------------------------------------------------------}

module BasicData where
  import Control.Monad.Trans.Reader
  import Data.Array.IO

  data Expr = Id Char
            | Num { val :: Float }
            | Int' Expr
            | Rnd Expr
            | Tab Expr
            | OrExpr Expr Expr
            | AndExpr Expr Expr
            | NotExpr Expr
            | CompareExpr String Expr Expr
            | MultExpr Char Expr Expr
            | AddExpr Char Expr Expr
            | NegateExpr Expr
            | PowerExpr Expr Expr
            | Array' Expr [Expr]
            | String' { str :: String }
            | SepExpr String Expr

  instance Show Expr where
    show (Id c) =  [c]
    show (Num n) = showNum n
    show (Int' e) = "INT(" ++ show e ++ ")"
    show (Rnd e) = "RND(" ++ show e ++ ")"
    show (Tab e) = "TAB(" ++ show e ++ ")"
    show (OrExpr lhs rhs) = show lhs ++ " OR " ++ show rhs
    show (AndExpr lhs rhs) = show lhs ++ " AND " ++ show rhs
    show (NotExpr e) = "NOT " ++ show e
    show (CompareExpr op lhs rhs) = show lhs++ " " ++ op ++ " " ++ show rhs
    show (MultExpr c lhs rhs) = show lhs ++ " " ++ [c] ++ " " ++ show rhs
    show (AddExpr c lhs rhs) = show lhs ++ " " ++ [c] ++ " " ++ show rhs
    show (NegateExpr e) = "-" ++ show e
    show (PowerExpr base power) = show base ++ " ^ " ++ show power
    show (Array' e es) = show e ++ "(" ++ showSepList "," es ++ ")"
    show (String' s) = "\"" ++ s ++ "\""

  showNum :: Float -> String
  showNum f = do
    let i = fromIntegral $ truncate f
    if (f - i) == 0
      then show $ truncate i -- show int
      else show f -- show float

  showSepList :: String -> [Expr] -> String
  showSepList _ [] = ""
  showSepList _ [e] = show e
  showSepList s (e:es) = show e ++ s ++ showSepList s es

  data Statement = DIM [Expr]
                 | END
                 | FOR Expr Expr Expr (Maybe Expr)
                 | GOSUB Expr
                 | GOTO Expr
                 | IFTHEN Expr Expr
                 | INPUT String [Expr]
                 | LET Expr Expr
                 | NEXT [Expr]
                 | ONGOTO Expr [Expr]
                 | PRINT [Expr]
                 | REM String
                 | RETURN

  instance Show Statement where
    show (DIM as) = "DIM " ++ showExprList as
    show END = "END"
    show (FOR id init lim Nothing) = "FOR " ++ show id ++ " = " ++ show init ++ " TO " ++ show lim
    show (FOR id init lim (Just step)) = "FOR " ++ show id ++ " = " ++ show init ++ " TO " ++ show lim ++ " STEP " ++ show step
    show (GOSUB ln) = "GOSUB " ++ show ln
    show (GOTO ln) = "GOTO " ++ show ln
    show (IFTHEN e line) = "IF " ++ show e ++ " THEN " ++ show line
    show (INPUT "" exprs) = "INPUT" ++ showExprList exprs
    show (INPUT str exprs) = "INPUT " ++ show str ++ "; " ++ showExprList exprs
    show (LET lhs rhs) = "LET " ++ show lhs ++ " = " ++ show rhs
    show (NEXT exprs) = "NEXT " ++ showSepList "," exprs
    show (ONGOTO e nums) = "ON " ++ show e ++ " GOTO " ++ showSepList ", " nums
    show (PRINT exprs) = "PRINT " ++ showExprList exprs
    show (REM str) = "REM " ++ str
    show RETURN = "RETURN"

  showExprList :: [Expr] -> String
  showExprList [] = ""
  showExprList ((SepExpr d e):es) = show e ++ d ++ " " ++ showExprList es
  showExprList (e:es) = show e ++ showExprList es

  data BasicLine = Line { lineNumber :: Int, statements :: [Statement] }

  instance Show BasicLine where
    show (Line line stmts) = show line ++ " " ++ showStatements stmts
  
  showStatements :: [Statement] -> String
  showStatements [] = ""
  showStatements [s] = show s
  showStatements (s:ss) = show s ++ ": " ++ showStatements ss

  printBasicLines :: [BasicLine] -> IO ()
  printBasicLines [] = return ()
  printBasicLines (b:bs) = do { print b; printBasicLines bs }

  -- Program is an array of BasicLines
  type Program = [BasicLine]

  -- Vars
  type Vars = IOArray Char (Maybe Float)

  -- Arrays is an array that stores all float variables
  type Array1D = IOArray Int Float
  type Array2D = IOArray Int Array1D
  type Arrays = IOArray Char (Maybe Array2D)

  -- CurrentLine is an array of size 1 that holds the current line number
  type CurrentLine = IOArray Int Int

  -- The Environment is a tuple of the program, variables, and current line
  type Environment = (Program, Vars, CurrentLine, Arrays)

  -- The Interpreter is a ReaderT that holds the Environment of the program
  type Interpreter = ReaderT Environment IO
