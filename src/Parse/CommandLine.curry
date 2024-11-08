module Parse.CommandLine where

import Data.Char             ( toUpper )
import System.Environment    ( getProgName, getArgs )
import System.Console.GetOpt

import Types

-- Options
options :: [OptDescr Flag]
options =
  [ Option "h?" ["help"]   (NoArg  Help)           "print help and exit"
  , Option "i"  ["ignore"] (ReqArg Ignore "CHECK") "ignores given style check"
  , Option "a"  ["add"]    (ReqArg Add    "CHECK") "applies given style check"
  , Option "o"  ["output"] (ReqArg checkOType "TYPE" )
           "determines output type of messages\nwhere TYPE is TEXT (default) or JSON"
  , Option "v" ["verbosity"] (ReqArg (\l -> Verbosity (read l)) "LEVEL" )
     ("verbosity level:\n0: quiet (warning only)\n" ++
      "1: (default, show: module, hints, code)\n" ++
      "2: (verbose, show infos and warnings)\n3: show all options and details")
  ]
 where
  checkOType s =
    let us = map toUpper s
    in if us `elem` ["JSON","TEXT"]
         then OType us
         else error $ "Illegal output type `" ++ s ++ "' (try `-h' for help)"

-- Returns for a list of program parameters a list of flags
-- and further (unprocessed) parameters.
parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
  case getOpt Permute options argv of
    -- getOpts with no fixed order, show possible options, if error occured
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageText))

-- Help text
usageText :: String
usageText = usageInfo header options
 where header = "Usage: curry-stylecheck [OPTION...] files..."
