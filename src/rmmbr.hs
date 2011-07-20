--Filename: rmmbr.hs
--Project: Rmmbr, a command-line program for handling reminders
--Author: Spencer Gordon
--Date: July 11th, 2011

module Main where

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Locale
import System.Exit
import System.Directory
import Data.Time
import Data.Maybe( fromMaybe )
import Data.Version
import Toolbox( inTwoColumns )

version_log = [Version [0,3] ["fixed_io"]
              ,Version [0,4] ["almost_stable"]
              ,Version [0,5] ["getting_there"]
              ,Version [0,6] ["finalized_io"]
              ]

appNamePretty = "Rmmbr"
appName = "rmmbr"
configFilePath = "rmmbr.config"
defaultFile = "todo.txt"
decoLength = 30 
defaultSort = "def"
defaultList = "todo"
usageHelpHeader = "Usage: rmmbr [OPTION...] "

commands :: [(String, ((Options, [String]) -> IO (), [OptDescr OptionTransformer]))]
commands = [ ("show", (present, presentOptions))
           , ("add", (add, addOptions))
           , ("remove", (remove, removeOptions))
           , ("create", (create, createOptions))
           , ("help", (help, helpOptions))
           , ("reset", (reset, resetOptions))
           ]

command_info = [ ("show", "view the contents of your todo lists")
               , ("add", "add an entry to one of your todo lists")
               , ("remove", "remove an entry from one of your todo lists")
               , ("create", "create a new todo list")
               , ("help", "show this help information")
               , ("reset", "clear all your todo lists, and start over with no entries")
               ]

data Options = Options { showVerbose :: Bool,
                         showHelp :: Bool,
                         theList :: String,
                         overwrite :: Bool,
                         position :: String,
                         entry :: String,
                         importance :: String,
                         confirm :: String,
                         sort :: String }

defaultOptions :: Options
defaultOptions = Options { showVerbose = False,
                           showHelp = False,
                           theList = defaultList,
                           overwrite = False,
                           position = "",
                           entry = "",
                           importance = "",
                           sort = "" }

{-- all the optionTransformer actions are defined here --}
type OptionTransformer = Options -> IO Options

doVersion :: Options -> IO Options
doVersion opts = do putStrLn $ unlines $ map showVersion version_log
                    return opts 

doVerbose :: Options -> IO Options
doVerbose opts = do putStrLn "doVerbose" 
                    return opts { showVerbose = True }

doHelp :: Options -> IO Options
doHelp opts = do putStrLn "doHelp" 
                 return opts { showHelp = True }

doOverwrite :: Options -> IO Options
doOverwrite opts = return opts { overwrite = True }

doRemoveList :: String -> Options -> IO Options
doRemoveList removeName opts = return opts { theList = removeName } 

doPosition :: String -> Options -> IO Options
doPosition pos opts = return opts { position = pos }

doEntry :: String -> Options -> IO Options
doEntry text opts = return opts { entry = text }

doAddList :: String -> Options -> IO Options
doAddList list opts = return opts { theList = list }

doImportance :: String -> Options -> IO Options
doImportance level opts = return opts { importance = level }

doShowList :: String -> Options -> IO Options
doShowList list opts = return opts { theList = list }

doSort :: String -> Options -> IO Options
doSort sortBy opts = return opts { sort = sortBy }

{-- Different commonly occurring options will be defined here so they can be reused --}

commandHelpOption = Option ['h'] ["help"] (NoArg doHelp) "get help for this command"
versionOption = Option ['V'] ["version"] (NoArg doVersion) "show detailed version information, with changes from previous versions"
verboseOption desc = Option ['v'] ["verbose"] (NoArg doVerbose) desc
programHelpOption = Option ['h'] ["help"] (NoArg doHelp) "show help information for this program"
selectListOption = Option ['l'] ["list"] (ReqArg doShowList "LIST") "choose a list to display"
sortOrderOption = Option ['s'] ["sort"] (ReqArg doSort "SORT ORDER") "specify an order for entries to be sorted in when they are presented"

{-- the main function --}

main = do putStr $ appNamePretty ++ ": "
          appDir <- getAppUserDataDirectory appName
          alreadyConfigured <- doesDirectoryExist appDir
          if alreadyConfigured then return () else configure appDir
          input <- getArgs
          let action = if null input then present 
                                     else case lookup (head input) commands of
                                          Just (act, _) -> act
                                          Nothing -> present
          let args = if null input then []
                                   else case lookup (head input) commands of
                                        Just _ -> (tail input)
                                        Nothing -> input
          let options = if null input then []
                                      else case lookup (head input) commands of
                                           Just (_, opt) -> opt
                                           Nothing -> presentOptions
          processedInput <- processInput args options
          action processedInput

presentOptions :: [OptDescr OptionTransformer]
presentOptions = [selectListOption
                 ,verboseOption "show more detailed version of selected lists"
                 ,sortOrderOption
                 ,commandHelpOption
                 ,versionOption
                 ]

processInput :: [String] -> [OptDescr OptionTransformer] -> IO (Options, [String])
processInput args options = do case getOpt RequireOrder options args of
                                    (optionTransformations, nonOpts, []) -> do opts <- foldl (>>=) (return defaultOptions) optionTransformations
                                                                               return (opts, nonOpts)
                                    (_,_,errors) -> ioError $ userError $ ((unlines errors) ++ (usageInfo usageHelpHeader options))

{-- Shows a list or group of lists --}

present :: (Options, [String]) -> IO ()
present (opts,_) = do putStrLn "Showing all entries."
                      let chosenList = theList opts ++ ".txt"
                      appDir <- getAppUserDataDirectory appName
                      allLists <- getKnownLists configFilePath appDir                                                    
                      if chosenList `elem` allLists then do setCurrentDirectory appDir
                                                            withFile chosenList ReadMode (\handle -> do contents <- hGetContents handle
                                                                                                        putStrLn contents)
                                                    else ioError $ userError $ "The selected list does not exist: " ++ chosenList

addOptions :: [OptDescr OptionTransformer]
addOptions = [commandHelpOption
             ,selectListOption
             ]

add :: (Options, [String]) -> IO ()
add (opts,_) = do putStrLn "Adding an entry."
                  let chosenList = theList opts ++ ".txt"
                      newEntry = entry opts   
                  appDir <- getAppUserDataDirectory appName                   
                  allLists <- getKnownLists configFilePath appDir                     
                  currentTime <- getCurrentTime
                  if chosenList `elem` allLists then do setCurrentDirectory appDir
                                                        withFile chosenList ReadWriteMode (\handle -> do hPutStr handle $ formatTime defaultTimeLocale "\n%D (%X) - " currentTime 
                                                                                                         hPutStrLn handle newEntry)
                                                else ioError $ userError "The selected list does not exist"

removeOptions :: [OptDescr OptionTransformer]
removeOptions = [commandHelpOption
                ]

remove :: (Options, [String]) -> IO ()
remove (opts,_) = do putStrLn "Removing an entry."
                     return ()

createOptions :: [OptDescr OptionTransformer]
createOptions = [commandHelpOption
                ]

create :: (Options, [String]) -> IO ()
create (opts,_) = do putStrLn "Creating a new list of entries."
                     return ()

helpOptions :: [OptDescr OptionTransformer]
helpOptions = [commandHelpOption
              ]

help :: (Options, [String]) -> IO ()
help (opts,_) = do putStrLn "Displaying help for the program."
                   putStrLn $ inTwoColumns command_info
                       
resetOptions :: [OptDescr OptionTransformer]
resetOptions = [
               ]

reset :: (Options, [String]) -> IO ()
reset (opts,_) = do putStrLn "Displaying help for the reset command"
                    return ()

configure :: FilePath -> IO ()
configure appDir = do putStrLn "Creating appropriate directories and config files..."
                      createDirectoryIfMissing True appDir
                      setCurrentDirectory appDir
                      writeFile configFilePath ""
                      writeFile defaultFile ((replicate decoLength '*') ++ "\ntodo.txt\n" ++ (replicate decoLength '*'))


getKnownLists :: FilePath -> String -> IO [FilePath]
getKnownLists configFilePath appDir= do setCurrentDirectory appDir
                                        configFile <- openFile configFilePath ReadMode
                                        lists <- getListsFromConfig configFile
                                        return lists
                      
getListsFromConfig :: Handle -> IO [FilePath]
getListsFromConfig file = do list <- getListsFromConfigHelper file
                             return $ let isFile x = case x of 
                                                       Just path -> True
                                                       Nothing -> False
                                          extract :: Maybe a -> a 
                                          extract (Just something) = something
                                      in map extract (filter isFile list)

getListsFromConfigHelper :: Handle -> IO [Maybe FilePath]
getListsFromConfigHelper file = do doneReading <- hIsEOF file
                                   if not doneReading 
                                      then do head <- (readLineFromConfig file)
                                              rest <- getListsFromConfigHelper file 
                                              let result = head:rest
                                              return result
                                      else do let result = (Just defaultFile):[]
                                              return result
readLineFromConfig :: Handle -> IO (Maybe FilePath)
readLineFromConfig handle =  do line <- hGetLine handle 
                                if (head (words line)) == "FILE" then return (Just ((words line) !! 2))
                                                                 else return Nothing