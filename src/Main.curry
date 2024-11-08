module Main where

import System.Environment ( getProgName, getArgs )
import System.CurryPath   ( lookupModuleSourceInLoadPath, stripCurrySuffix )
import System.Directory   ( doesFileExist, getHomeDirectory )
import System.FilePath    ( (</>) )
import Control.Monad      ( when )

import Curry.Files        ( readFullAST )
import Curry.Types
import Curry.Position
import Curry.SpanInfo
import Curry.Span
import Curry.Ident

import Parse.CommandLine         ( parseOpts, usageText )
import Parse.Config              ( parseConfig, defaultConfig )
import Control.Monad.Trans.State 
import Types
import Check
import Pretty.ToString           ( renderMessagesToString )
import Pretty.ToJson             ( renderMessagesToJson )
import Pretty.ShowOptions        ( showOptions )

import Prelude hiding ( ifThenElse )

-- Banner of this tool:
scBanner :: String
scBanner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "Curry Style Checker (Version of 30/10/2024)"
   bannerLine = take (length bannerText) (repeat '-')

-- URL containing Curry style guide:
styleGuideURL :: String
styleGuideURL = "http://www.informatik.uni-kiel.de/~curry/style/"

-- Commandline tool.
main :: IO ()
main = getCheckOpts >>= styleCheck

-- Starts programm with arguments from commandline,
-- if the file to check is found, finds config, if specified in
-- commandlineoptions. If config not used or not found, uses defaultConfig.
-- Gets `spanAST` and src of code and runs checkAll on these two files,
-- returned String (Messages) are put into console output.
styleCheck :: Arguments -> IO ()
styleCheck a@(_, flags, _) = do
  config <- getConfig flags >>= updateConfigWithOpts flags
  restrict config 1 scBanner
  restrict config 3 (showOptions config)
  if Help `elem` flags
    then putStrLn $ usageText ++ "\nSee also the Curry Style Guide at\n\n    "
                    ++ styleGuideURL
    else styleCheck' a config

styleCheck' :: Arguments -> Config -> IO ()
styleCheck' (_, _, []) config =
  restrict config 1 "All given files checked.\n"
styleCheck' (p, o, (fileName:files)) config  = do
  let modName = stripCurrySuffix fileName
  restrict config 2 $ "--------------------------------\n"
                      ++ "INFO: Reading module " ++ modName
  filePaths <- lookupModuleSourceInLoadPath modName
  case filePaths of
    Nothing           -> do putStrLn $ "WARNING: "
                                       ++ modName
                                       ++ " does not exist\n"
                            styleCheck' (p, o, files) config
    Just (_,filePath) -> do
      ast <- getAST modName config
      src <- getSrc filePath config
      restrict config 2  $ "INFO: Checking style of file " ++ modName
      messages <- return (checkAll src ast config modName (getOutputOption config))
      restrict config 1 $ "--------------------------------\n"
                ++ modName ++ "\n"
                ++ "--------------------------------\n"
      restrict config 0 $ messages ++"\n"
      styleCheck' (p, o, files) config

-- Determines output function by configuration.
getOutputOption :: Config -> (Config -> String -> [SrcLine] -> [Message] -> String)
getOutputOption c = case oType c of
  JSON -> renderMessagesToJson
  TEXT -> renderMessagesToString

-- Updates config after reading the currystylecheckrc by updating according to the flag.
updateConfigWithOpts :: [Flag] -> Config -> IO Config
updateConfigWithOpts []     conf = return conf
updateConfigWithOpts (f:fs) conf@(Config checks out verb hint code maxLength) = case f of
  (Ignore s)              -> do
    newCheckl <- updateChecks s checks False conf
    updateConfigWithOpts fs (Config newCheckl out verb hint code maxLength)
  (Add s)                 -> do
    newCheckl <- updateChecks s checks True conf
    updateConfigWithOpts fs (Config newCheckl out verb hint code maxLength)
  (OType "JSON")          ->
    updateConfigWithOpts fs (conf {oType = JSON})
  (OType "TEXT")          ->
    updateConfigWithOpts fs (conf {oType = TEXT})
  (Verbosity i)           ->
    updateConfigWithOpts fs (conf {verbosity = (if ((i < 4) && (i > -1)) then i else 1)})
  _                       -> updateConfigWithOpts fs conf

-- Updates one check according to given string (check name) and bool.
updateChecks :: String -> CheckList -> Bool -> Config -> IO CheckList
updateChecks s checkl b c = case s of
  "tabs"              -> return checkl {tab = b}
  "lineLength"        -> return checkl {lineLength = b}
  "ifThenElse"        -> return checkl {ifThenElse = b}
  "case"              -> return checkl {caseIndent = b}
  "do"                -> return checkl {doIndent = b}
  "let"               -> return checkl {letIndent = b}
  "guard"             -> return checkl {guardIndent = b}
  "functionRhs"       -> return checkl {rhsAlign = b}
  "equalsTrue"        -> return checkl {equalstrue = b}
  "signatures"        -> return checkl {topLevelSig = b}
  "blankLines"        -> return checkl {blankLines = b}
  "trailingSpaces"    -> return checkl {trailingS = b}
  "whiteSpaces"       -> return checkl {whiteSpace = b}
  "moduleHeader"      -> return checkl {moduleheader = b}
  "imports"           -> return checkl {imports = b}
  "data"              -> return checkl {dataIndent = b}
  "list"              -> return checkl {listIndent = b}
  "thentrueelsefalse" -> return checkl {thenTrueElseFalse = b}
  "notEqual"          -> return checkl {notEqual = b}
  "notOrd"            -> return checkl {notOrd = b}
  "equalsEmptyList"   -> return checkl {equalsEmptyList = b}
  "identFunc"         -> return checkl {identFunc = b}
  "constFunc"         -> return checkl {constFunc = b}
  "andOr"             -> return checkl {andOr = b}
  "print"             -> return checkl {printCheck = b}
  "deriving"          -> return checkl {derivingIndent = b}
  "class"             -> return checkl {classIndent = b}
  "instance"          -> return checkl {instanceIndent = b}
  _                   -> do restrict c 2
                              ( "WARNING: tried to "
                                ++ (if b then "add" else "ignore")
                                ++ " an invalid check ´"
                                ++ s
                                ++ "´, passing over" )
                            return checkl

-- Only print `s` if the current verbosity isn't lower than its restriction.
restrict :: Config -> Int -> String -> IO ()
restrict conf i s = when ((verbosity conf) >= i)
                         (putStrLn s)

------------------------------------------------------------------------------
-- Name of the config file for the style checker.
configFileName :: String
configFileName = "currystylecheckrc"

-- Loads currystylecheckrc first from local directory, if not existing
-- from home and at last creates a default configuration.
getConfig :: [Flag] -> IO Config
getConfig flags = do
  iconfig <- updateConfigWithOpts flags defaultConfig
  home <- getHomeDirectory
  configExistsHere <- doesFileExist (configFileName)
  if configExistsHere
    then parseConfig (verbosity iconfig > 1) (configFileName)
    else do
      restrict iconfig 2 $ "INFO: config file not found in current directory,"
                           ++ " searching home directory"
      configExistsHome <- doesFileExist $ home </> configFileName
      if configExistsHome
        then parseConfig (verbosity iconfig > 1) $ home </> configFileName
        else do
          restrict iconfig 2 $ "INFO: config file not found in home directory,"
                               ++ " using default settings"
          return defaultConfig

-- Gets filename and config,
-- if any check on AST is on (config), then get the Ast of the programm
-- if the curry file has a suffix .curry, remove it to find corresponding
-- AST.
-- If no check needed, returns an empty module to avoid reading time of SpanAST.
getAST :: String -> Config -> IO (Module ())
getAST fileName config =
  if (anyAST config)
    then do restrict config 2 $ "INFO: Getting SpanAST of " ++ fileName
            ast <- readFullAST fileName
            const (return ast) $!! ast
    else return (Module
                  (SpanInfo (Span (Position 1 1) (Position 1 1)) [])
                  WhitespaceLayout
                  []
                  (ModuleIdent NoSpanInfo ["NoAST"])
                  Nothing
                  []
                  []
                )

-- Gets filename and config, if any check on src is on (config),
-- return the sourcecode in form of a list of lines, which
-- are indexed strings.
getSrc :: String -> Config -> IO [(Int,String)]
getSrc fileName config =
  if (anySrc config)
    then do restrict config 2 $ "INFO: Parsing file " ++ fileName
            ls <- readFile (fileName) >>= return . lines
            let src = zip [1..(length ls)] ls
            return $ filter (\(_,l) -> (length l) > 0) src
    else return []

-- Retrieves the program's name and commandline arguments,
-- parses and returns name, flags and further informations.
getCheckOpts :: IO Arguments
getCheckOpts = do
  args  <- getArgs
  prog  <- getProgName
  (o,n) <- parseOpts args
  return (prog, o, n)
