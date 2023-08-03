{-----------------------------------------------------------------------------
 - 
 - Variables.hs
 -
 - Christopher Salinas
 - Gavin McGuire
 -
 - CS 456
 - Fall 2020
 - University of New Mexico
 -
 -----------------------------------------------------------------------------}

module Variables where
  import BasicData
  import Control.Monad
  import Control.Monad.IO.Class
  import Control.Monad.Trans.Reader
  import Data.Array.Base
  import System.IO

  -- Variable
  setVariable :: Char -> Float -> Interpreter ()
  setVariable c v = do
    (_, vars, _, _) <- ask
    liftIO $ writeArray vars c $ Just v

  getVariable :: Char -> Interpreter (Maybe Float)
  getVariable c = do
    (_, vars, _, _) <- ask
    liftIO $ readArray vars c

  clearVariable :: Char -> Interpreter ()
  clearVariable c = do
    (_, vars, _, _) <- ask
    liftIO $ writeArray vars c Nothing

  -- 1D Array Variable
  create1D :: Int -> IO Array1D
  create1D w = newArray (0,w-1) 0

  dim1D :: Char -> Int -> Interpreter ()
  dim1D c w = do
    (_, _, _, arrs) <- ask
    arr <- liftIO $ create2D 1 (w+1)
    liftIO $ writeArray arrs c $ Just arr
  
  setVariable1D :: Char -> Int -> Float -> Interpreter ()
  setVariable1D c = setVariable2D c 0

  getVariable1D :: Char -> Int -> Interpreter (Maybe Float)
  getVariable1D c = getVariable2D c 0

  -- Create a 2D array
  -- 1. Repeat w, h times. For example: h=2, w=3 => [3, 3]
  -- 2. Map create1D over each of the elements in the list from step 1
  -- 3. Create a new array using the list from step 2
  -- Result is a 2x3 matrix:
  --   Nothing, Nothing, Nothing
  --   Nothing, Nothing, Nothing
  create2D :: Int -> Int -> IO Array2D
  create2D h w = do
    l1 <- replicateM h (create1D w)
    newListArray (0,h-1) l1

  -- Initialize a 2D array for the variable and size given
  -- This creates a 2D array of size h+1 by w+1. It has indices (0,h) and (0,w)
  dim2D :: Char -> Int -> Int -> Interpreter ()
  dim2D c h w = do
    (_, _, _, arrs) <- ask
    arr <- liftIO $ create2D (h+1) (w+1)
    liftIO $ writeArray arrs c $ Just arr

  -- ID(I,J) = V
  setVariable2D :: Char -> Int -> Int -> Float -> Interpreter ()
  setVariable2D c i j v = do
    (_, _, _, arrs) <- ask
    ma2 <- liftIO $ readArray arrs c
    case ma2 of
      Nothing -> error $ "Found Nothing for " ++ [c]
      Just a2 -> do
        a1 <- liftIO $ readArray a2 i
        liftIO $ writeArray a1 j v

  -- Get value at ID(I,J)
  getVariable2D :: Char -> Int -> Int -> Interpreter (Maybe Float)
  getVariable2D c i j = do
    (_, _, _, arrs) <- ask
    ma2 <- liftIO $ readArray arrs c
    case ma2 of
      Nothing -> return Nothing
      Just a2 -> do
        a1 <- liftIO $ readArray a2 i
        v <- liftIO $ readArray a1 j
        return $ Just v