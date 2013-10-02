-- | Main Program reads the command line options, checks the supplied
-- sif file, and pushes the dot version out to STDOUT.
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit

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
  putStrLn "Read in AST for file"
  case parseSif contents of
    Left err -> do
         putStrLn $ file opts ++ " failed to parse."
         print $ show err
         exitWith (ExitFailure 1)
    Right ast' -> do
         putStrLn $ case ast opts of
                      True -> show ast'
                      otherwise -> ""

         -- Type Check it.
         putStrLn "Converting AST to Model\n"
         case chkPlangSpec ast' of
           Left err -> do
                   putStrLn "Specification does not type check"
                   print $ show err
                   exitWith (ExitFailure 1)
           Right res -> do
                   putStrLn $ "Pattern language "
                              ++ show (Model.title res)
                              ++ " " 
                              ++ "type checks"
                   case model opts of
                     True -> print res
                     otherwise -> putStrLn ""
                      
                   -- If requested print the transformation
                   -- Dirty Dirty Code that needs re doing.
                   case to opts of
                     "dot"     -> print $ plang2Dot res
                     "sif"     -> print $ plang2Sif res
                     ""        -> putStrLn ""
                     otherwise -> error "Unsupported Format"
         
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
             &= details ["Sif is a Statically typed DSL for the specification of software system pattern languages. The Sif binary provides both a type-checker and transformation tool for Sif instances.", "", "The type-checker checks a Sif instance for both syntaxtic and semantic correctness. Please consult the specification for the typing rules.", "", "The transformation aspect allows for well typed Sif files to be convert into an alternate format. Supported transformations are: Dot and Sif.", "Caveat: Imports are not checked for now. Sorry!"]

-- --------------------------------------------------------------------- [ EOF ]
