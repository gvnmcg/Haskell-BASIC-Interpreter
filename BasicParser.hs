{-----------------------------------------------------------------------------
 - 
 - BasicParser.hs
 -
 - Christopher Salinas
 - Gavin McGuire
 -
 - CS 456
 - Fall 2020
 - University of New Mexico
 -
 -----------------------------------------------------------------------------}

module BasicParser where
  import BasicData
  import Data.Array.IO
  import Data.Char
  import Parselib
  import System.IO

  -- LISTS
  parseStatements :: Parser [Statement]
  parseStatements = parseStatement `sepby` symb ":"

  parseIdList :: Parser [Expr]
  parseIdList = parseId `sepby` symb ","

  parseNumList :: Parser [Expr]
  parseNumList = parseNumber `sepby` symb ","

  parseExpressionList :: Parser [Expr]
  parseExpressionList = parseExpr `sepby` symb ","

  parseArrayList :: Parser [Expr]
  parseArrayList = parseArray `sepby` symb ","

  parsePrintList :: Parser [Expr]
  parsePrintList = parseCommaList
    +++ parseSemiColonList
    +++ parseNoSepList
    +++ return []

  parseCommaList :: Parser [Expr]
  parseCommaList = do
    e <- parseExpr
    symb ","
    es <- parsePrintList
    return $ SepExpr "," e : es
  
  parseSemiColonList :: Parser [Expr]
  parseSemiColonList = do
    e <- parseExpr
    symb ";"
    es <- parsePrintList
    return $ SepExpr ";" e : es

  parseNoSepList :: Parser [Expr]
  parseNoSepList = do
    e <- parseExpr
    return [e]

  -- STATEMENT PARSERS
  parseStatement :: Parser Statement
  parseStatement = parseDim
    +++ parseEnd
    +++ parseLet
    +++ parseForStep +++ parseFor
    +++ parseGoSub
    +++ parseGoTo
    +++ parseIfThen
    +++ parseInputString +++ parseInput
    +++ parseNext
    +++ parseOnGoTo
    +++ parsePrint
    +++ parseRem
    +++ parseReturn

  parseDim :: Parser Statement
  parseDim = do
    symb "DIM"
    DIM <$> parseArrayList

  parseEnd :: Parser Statement
  parseEnd = do
    symb "END"
    return END

  parseLet :: Parser Statement
  parseLet = do { symb "LET"; parseAssignment } +++ parseAssignment

  parseAssignment :: Parser Statement
  parseAssignment = do
    var <- parseVariableExpr
    symb "="
    LET var <$> parseExpr

  parsePrint :: Parser Statement
  parsePrint = do
    symb "PRINT"
    PRINT <$> parsePrintList

  parseInput :: Parser Statement
  parseInput = do
    symb "INPUT"
    INPUT "" <$> parseIdList

  parseInputString :: Parser Statement
  parseInputString = do
    symb "INPUT"
    e <- parseString
    symb ";"
    INPUT (str e) <$> parseIdList

  parseIfThen :: Parser Statement
  parseIfThen = do
    symb "IF"
    e <- parseExpr
    symb "THEN"
    IFTHEN e <$> parseExpr

  parseForStep :: Parser Statement
  parseForStep = do
    symb "FOR"
    var <- parseId
    symb "="
    val <- parseExpr
    symb "TO"
    lim <- parseExpr
    symb "STEP"
    FOR var val lim . Just <$> parseExpr

  parseFor :: Parser Statement
  parseFor = do
    symb "FOR"
    var <- parseId
    symb "="
    val <- parseExpr
    symb "TO"
    lim <- parseExpr
    return $ FOR var val lim Nothing

  parseNext :: Parser Statement
  parseNext = do
    symb "NEXT"
    NEXT <$> parseIdList

  parseOnGoTo :: Parser Statement
  parseOnGoTo = do
    symb "ON"
    e <- parseExpr
    symb "GOTO"
    ONGOTO e <$> parseNumList

  parseGoTo :: Parser Statement
  parseGoTo = do
    symb "GOTO"
    GOTO <$> parseExpr

  parseGoSub :: Parser Statement
  parseGoSub = do
    symb "GOSUB"
    GOSUB <$> parseExpr
  
  parseReturn :: Parser Statement
  parseReturn = do
    symb "RETURN"
    return RETURN

  parseRem :: Parser Statement
  parseRem = do
    symb "REM"
    str <- many $ sat isPrint
    return $ REM str

  -- EXPRESSION PARSERS
  parseExpr :: Parser Expr
  parseExpr = parseOrExpr +++ andExpr

  parseOrExpr :: Parser Expr
  parseOrExpr = do
    a <- andExpr
    symb "OR"
    OrExpr a <$> parseExpr

  andExpr = parseAndExpr +++ parseNotExpr

  parseAndExpr :: Parser Expr
  parseAndExpr = do
    n <- parseNotExpr
    symb "AND"
    AndExpr n <$> parseExpr

  parseNotExpr :: Parser Expr
  parseNotExpr = parseNot +++ parseCompareExpr

  parseNot :: Parser Expr
  parseNot = do
    symb "NOT"
    NotExpr <$> parseCompareExpr

  parseCompareExpr :: Parser Expr
  parseCompareExpr = parseCompare "=" 
    +++ parseCompare "<>"
    +++ parseCompare ">"
    +++ parseCompare ">="
    +++ parseCompare "<"
    +++ parseCompare "<="
    +++ parseAddExpr
  
  parseCompare :: String -> Parser Expr
  parseCompare op = do
    m <- parseAddExpr
    symb op
    CompareExpr op m <$> parseCompareExpr

  parseAddExpr :: Parser Expr
  parseAddExpr = parseAdd +++ parseSub +++ parseMultExpr

  parseAdd :: Parser Expr
  parseAdd = do
    m <- parseMultExpr
    symb "+"
    AddExpr '+' m <$> parseAddExpr

  parseSub :: Parser Expr
  parseSub = do
    m <- parseMultExpr
    symb "-"
    AddExpr '-' m <$> parseAddExpr

  parseMultExpr :: Parser Expr
  parseMultExpr = parseMultTimes +++ parseMultDiv +++ parseNegateExpr

  parseMultTimes :: Parser Expr
  parseMultTimes = do
    n <- parseNegateExpr
    symb "*"
    MultExpr '*' n <$> parseMultExpr

  parseMultDiv :: Parser Expr
  parseMultDiv = do
    n <- parseNegateExpr
    symb "/"
    MultExpr '/' n <$> parseMultExpr

  parseNegateExpr :: Parser Expr
  parseNegateExpr = parseNegate +++ parsePowerExpr

  parseNegate :: Parser Expr
  parseNegate = do
    symb "-"
    NegateExpr <$> parsePowerExpr

  parsePowerExpr :: Parser Expr
  parsePowerExpr = parsePower +++ parseValue

  parsePower :: Parser Expr
  parsePower = do
    v <- parseValue
    symb "^"
    PowerExpr v <$> parsePowerExpr

  parseValue :: Parser Expr
  parseValue = parseParenExpr
    +++ parseFunctionExpr
    +++ parseVariableExpr
    +++ parseConstantExpr

  parseParenExpr :: Parser Expr
  parseParenExpr = do
    symb "("
    e <- parseExpr
    symb ")"
    return e

  parseFunctionExpr :: Parser Expr
  parseFunctionExpr = parseInt +++ parseRnd +++ parseTab

  parseInt :: Parser Expr
  parseInt = do
    symb "INT("
    e <- parseAddExpr
    symb ")"
    return $ Int' e
  
  parseRnd :: Parser Expr
  parseRnd = do
    symb "RND("
    e <- parseAddExpr
    symb ")"
    return $ Rnd e

  parseTab :: Parser Expr
  parseTab = do
    symb "TAB("
    e <- parseAddExpr
    symb ")"
    return $ Tab e

  parseVariableExpr :: Parser Expr
  parseVariableExpr = parseArray +++ parseId

  parseId :: Parser Expr
  parseId = do
    c <- token letter
    return $ Id c

  parseArray :: Parser Expr
  parseArray = do
    id <- parseId
    symb "("
    exprs <- parseExpressionList
    symb ")"
    return $ Array' id exprs

  parseConstantExpr :: Parser Expr
  parseConstantExpr = parseNumber +++ parseString

  parseNumber :: Parser Expr
  parseNumber = do
    Num . fromIntegral <$> nat

  parseString :: Parser Expr
  parseString = do
    char '"' -- only consume first quote
    str <- many $ sat (/= '"')
    symb "\"" -- consume second quote and spaces after
    return $ String' str

  -- PARSING 
  line :: Parser BasicLine
  line = do
    n <- nat -- line number
    Line n <$> parseStatements

  parseLine :: String -> BasicLine
  parseLine s = case apply line s of
    [(a,"")] -> a
    _ -> error ("Failed to parse line: " ++ s)

  parseProgram :: Handle -> IO Program
  parseProgram handle = do
    eof <- hIsEOF handle
    if eof 
      then return []
      else do
        s <- hGetLine handle
        let stmt = parseLine s
        stmts <- parseProgram handle
        return $ stmt:stmts
