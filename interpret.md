## foo.bas
```basic
[(10,LET A = 2)
,(20,LET B = 3)
,(30,LET C = 4)
,(40,PRINT A * B + C)
,(50,END)
]

)
```
```haskell

varState = 
    [('A', 2)
    ,('B', 3)
    ,('C', 4)
    ]

instrState = 
    [(PRINT, A * B + C),(END)]

```


A * B + C  
var * var + var  
val * val + val  

print expr

end? 

results: 
>
> 10
>


## test.bas

```
[(20,INPUT H),(25,LET X = INT(RND(1) * H + 1)),(27,PRINT X),(30,FOR I = 1 TO H),(35,PRINT I),(40,IF I = X THEN 60),(50,NEXT I),(60,END)]

State s =
(

(20,INPUT H)
,

[(25,LET X = INT(RND(1) * H + 1))
,(27,PRINT X)
,(30,FOR I = 1 TO H)
,(35,PRINT I)
,(40,IF I = X THEN 60)
,(50,NEXT I)
,(60,END)
]

)
```


```


module BasicInterpreter where
  import BasicData

  data Interpreter = Printer Num | Assignement

  interpret :: [(Int, Statement)] -> [String] -> IO ()
  interpret program args = do
    putStrLn $ show args
    putStrLn $ show program

  assign :: Interpreter
  assign = do
    (LET var val) <- let'
    put (var, val)
    return Assignement


  print :: Interpreter
  print = do
    (PRINT e) <- print'
    n <- nat
    return $ Printer e

```


```Haskell

type Environment = IOArray Char (Maybe Int)

```




- chipmunk basic
    - RND(N) returns random # in 1..N
    - blocker : file syntax error


- Program IOArray to do FOR, NEXT, GOTO
    - blocker : returning stmts array



- Program IOArray variables
    - blocker : a _Maybe_ typing confusion

          BasicInterpreter.hs:78:31: error:
            * Couldn't match expected type `Maybe Int'
                          with actual type `(a0 -> Int) -> Maybe a0 -> Int'
            * Probable cause: `maybe' is applied to too few arguments
              In the third argument of `writeArray', namely `(maybe val)'
              In a stmt of a 'do' block: writeArray env (chr var) (maybe val)
              In the expression:
                do var <- evalExpr l env
                  val <- evalExpr r env
                  writeArray env (chr var) (maybe val)

- floating points 
    - blocker : adding to the parserlib natural?





for 
- LET 
- save limiter



next
- 


FOR I = 1 TO H
==============
1. Assign I value of 1 (like a LET statement)
2. Compare if I <= H
    2-true. Continue a.k.a. return ()
    2-false. GOTO statement after NEXT

NEXT I
======
1. Increment I by 1
2. GOTO FOR statment


```

For id _ lim
|
|
|
V
Next id:ids
  - ^ id = Nothing  -> 
  - ^ id = #        ->
  - ^ id >= lim     ->


```