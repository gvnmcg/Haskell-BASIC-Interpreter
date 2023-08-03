{-----------------------------------------------------------------------------
 - 
 - BASIC.hs
 -
 - Christopher Salinas
 - Gavin McGuire
 -
 - CS 456
 - Fall 2020
 - University of New Mexico
 -
 -----------------------------------------------------------------------------}

import BasicInterpreter
import BasicParser
import System.IO
import System.Environment

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  program <- parseProgram handle
  hClose handle
  interpret program