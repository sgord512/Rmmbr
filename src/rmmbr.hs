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
              ]

appNamePretty = "Rmmbr"
appName = "rmmbr"
configFilePath = "rmmbr.config"
defaultFile = "todo.txt"
decoLength = 30 
defaultSort = "def"
defaultList = "todo"

commands :: [(String, ([String] -> [OptDescr OptionTransformer] -> IO (), [OptDescr OptionTransformer]))]
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
                         name :: String,
                         overwrite :: Bool,
                         removeList :: String,
                         position :: String,
                         entry :: String,
                         addList :: String,
                         importance :: String,
                         confirm :: String,
                         showList :: String,
                         sort :: String }

defaultOptions :: Options
defaultOptions = Options { showVerbose = False,
                           showHelp = False,
                           name = "",
                           overwrite = False,
                           removeList = "",
                           position = "",
                           entry = "",
                           addList = defaultList,
                           importance = "",
                           Main.showList = "",
                           sort = "" }

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

doName :: String -> Options -> IO Options
doName inputName opts = do putStrLn "doVerbose"
                           return opts { name = inputName }

doOverwrite :: Options -> IO Options
doOverwrite opts = return opts { overwrite = True }

doRemoveList :: String -> Options -> IO Options
doRemoveList removeName opts = return opts { removeList = removeName } 

doPosition :: String -> Options -> IO Options
doPosition pos opts = return opts { position = pos }

doEntry :: String -> Options -> IO Options
doEntry text opts = return opts { entry = text }


doAddList :: String -> Options -> IO Options
doAddList list opts = return opts { addList = list }

doImportance :: String -> Options -> IO Options
doImportance level opts = return opts { importance = level }

doShowList :: String -> Options -> IO Options
doShowList list opts = return opts { Main.showList = list }

doSort :: String -> Options -> IO Options
doSort sortBy opts = return opts { sort = sortBy }


{-- Different commonly occurring options will be defined here so they can be reused --}

commandHelpOption = Option ['h'] ["help"] (NoArg doHelp) "get help for this command"
versionOption = Option ['V'] ["version"] (NoArg doVersion) "show detailed version information, with changes from previous versions"
verboseOption desc = Option ['v'] ["verbose"] (NoArg doVerbose) desc
programHelpOption = Option ['h'] ["help"] (NoArg doHelp) "show help information for this program"
showListOption = Option ['l'] ["list"] (ReqArg doShowList "LIST") "choose a list to display"
sortOrderOption = Option ['s'] ["sort"] (ReqArg doSort "SORT ORDER") "specify an order for entries to be sorted in when they are presented"




-- OptDescr a = Option [Char] [String] (ArgDescr a) String

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
          action args options


presentOptions :: [OptDescr OptionTransformer]
presentOptions = [showListOption
                 ,verboseOption "show more detailed version of selected lists"
                 ,sortOrderOption
                 ,commandHelpOption
                 ,versionOption
                 ]

present :: [String] -> [OptDescr OptionTransformer] -> IO ()
present args options = do putStrLn "Showing all entries."
                          let (optionTransformations, nonOpts, msgs) = getOpt RequireOrder options args
                          opts <- foldl (>>=) (return defaultOptions) optionTransformations


addOptions :: [OptDescr OptionTransformer]
addOptions = [commandHelpOption
             ]

add :: [String] -> [OptDescr OptionTransformer] -> IO ()
add args options = do putStrLn "Adding an entry."
                      let (optionTransformations, nonOpts, msgs) = getOpt RequireOrder options args
                      opts <- foldl (>>=) (return defaultOptions) optionTransformations

{--
add [] = return ()
add (x:xs) = do putStrLn ("adding \"" ++ x ++ "\"")
                setCurrentDirectory appDir
                configFile <- openFile configFilePath ReadMode
                fileLocations <- getListsFromConfig configFile
                currentTime <- getCurrentTime
                hPutStr (head fileLocations) $ formatTime defaultTimeLocale "\n%D (%X) - " currentTime 
                hPutStrLn (head fileLocations) x
                addItems xs (head fileLocations)
--}

removeOptions :: [OptDescr OptionTransformer]
removeOptions = [commandHelpOption
                ]

remove :: [String] -> [OptDescr OptionTransformer] -> IO ()
remove args options = do putStrLn "Removing an entry."
                         let (optionTransformations, nonOpts, msgs) = getOpt RequireOrder options args
                         opts <- foldl (>>=) (return defaultOptions) optionTransformations


createOptions :: [OptDescr OptionTransformer]
createOptions = [commandHelpOption
                ]

create :: [String] -> [OptDescr OptionTransformer] -> IO ()
create args options = do putStrLn "Creating a new list of entries."
                         let (optionTransformations, nonOpts, msgs) = getOpt RequireOrder options args
                         opts <- foldl (>>=) (return defaultOptions) optionTransformations


helpOptions :: [OptDescr OptionTransformer]
helpOptions = [commandHelpOption
              ]

help :: [String] -> [OptDescr OptionTransformer] -> IO ()
help args options = do putStrLn "Displaying help for the program."
                       let (flags, nonOpts, msgs) = getOpt RequireOrder options args
                       opts <- foldl (>>=) (return defaultOptions) optionTransformations
                       putStrLn $ inTwoColumns command_info
                       
resetOptions :: [OptDescr OptionTransformer]
resetOptions = [
               ]

reset :: [String] -> [OptDescr OptionTransformer] -> IO ()
reset args options = do putStrLn "Displaying help for the reset command"
                        let (flags, nonOpts, msgs) = getOpt RequireOrder options args
                        opts <- foldl (>>=) (return defaultOptions) optionTransformations

configure :: FilePath -> IO ()
configure appDir = do putStrLn "Creating appropriate directories and config files..."
                      createDirectoryIfMissing True appDir
                      setCurrentDirectory appDir
                      writeFile configFilePath ""
                      writeFile defaultFile ((replicate decoLength '*') ++ "\ntodo.txt\n" ++ (replicate decoLength '*'))
                      
getListFromConfig :: Handle -> IO [FilePath]
getListFromConfig file = do list <- getListsFromConfigHelper file
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
{--
data Flags = Version
              | Verbose
              | Help
              | Name (String -> Flags)
              | Overwrite (Maybe String -> Flags)
              | RemoveList (Maybe String -> Flags)
              | Position (String -> Flags)
              | Entry (String -> Flags)
              | AddList (Maybe String -> Flags)
              | Importance (Maybe String -> Flags)
              | Confirm (String -> Flags)
              | ShowList String
              | Sort String

makeShowList :: Maybe String -> Flags
makeShowList ms = ShowList (fromMaybe defaultList ms)

makeSort :: Maybe String -> Flags
makeSort ms = Sort (fromMaybe defaultSort ms)
--}

{--
processArgs :: [FilePath] -> [String] -> IO ()
processArgs fileLocations [] = do putStrLn "Showing all recorded entries."
                                  itemsList <- readFile (head fileLocations)
                                  putStrLn (replicate decoLength '-')
                                  putStrLn itemsList
                                  putStrLn (replicate decoLength '-')
processArgs fileLocations (x:xs) | x == do putStrLn "Adding a new entry."
                                           listFile <- openFile (head fileLocations) AppendMode
                                           addItems args listFile 
                                           hClose listFile
                                 | otherwise 
--}