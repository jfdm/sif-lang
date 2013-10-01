-- | Main Program reads the command line options, checks the supplied
-- sif file, and pushes the dot version out to STDOUT.
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)

import qualified Sif.AST as AST
import qualified Sif.Model as Model

import Sif.Parser
import Sif.Checker
import Sif.Transform

-- | Entry point to the Sif Type Checker.
main :: IO ()
main = do 
  -- Get Args
  as <- getArgs
  opts <- (if null as 
           then withArgs ["--help"]
           else id) $ cmdArgs sifOptions

  contents <- readFile $ file opts
  putStrLn $ "Using file: " ++ file opts ++ "\n"

  -- Parse The File
  let resAST = parseSif contents
  putStrLn $ "Read in AST for file"
  putStrLn $ case ast opts of
               True -> show resAST
               otherwise -> ""

  -- Type Check it.
  let resModel = chkPlangSpec resAST
  putStrLn $ "Converting AST to Model"
  putStrLn $ case model opts of
               True -> show resModel
               otherwise -> ""

  putStrLn $ "Pattern language " ++ show (Model.title resModel) ++ " " ++ "type checks\n"

  -- Dirty Dirty Code that needs re doing.

  -- If requested print the transformation
  let resTrans = case to opts of
                   "dot" -> plang2Dot resModel
                   "sif" -> plang2Sif resModel
                   otherwise -> error "Unsupported Format"

  case to opts of 
    "" -> putStrLn ""
    otherwise -> print resTrans

  putStrLn "Caveat: Imports are not checked for now. Sorry!"


-- ----------------------------------------------------------------- [ Options ]

-- | Options the type checker takes.
data SifOptions = SifOptions {
      to     :: String,   -- ^ The output format
      output :: FilePath, -- ^ Output filename.
      ast    :: Bool,     -- ^ Print the AST
      model  :: Bool,     -- ^ Print the resulting model
      file   :: FilePath  -- ^ The input file.
    } deriving (Show, Data, Typeable)

-- | Set default options
sifOptions :: SifOptions
sifOptions = SifOptions {
               to = def
                 &= typ "FORMAT"
                 &= help "Format to output",
               output = def
                     &= typ "FILENAME"
                     &= help "The name of file to write the output too.",
               ast = def
                  &= help "Print the AST",
               model = def
                    &= help "Print the AST",
               file = def
                   &= typFile
                   &= argPos 0
             }
             &= summary "Sif (C) Jan de Muijnck-Hughes 2013"
             &= program "sif"

-- --------------------------------------------------------------------- [ EOF ]
