--Filename: rmmbr.hs
--Project: Rmmbr, a command-line program for handling reminders
--Author: Spencer Gordon
--Date: July 22th, 2011

module Main where

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Locale
import System.Exit
import System.Directory
import Data.Time
import Monad
import qualified Data.List as List
import Data.Maybe( fromMaybe, isNothing )
import Data.Version
import Toolbox( inTwoColumns )

version_log = [Version [0,3] ["fixed io"]
              ,Version [0,4] ["almost stable"]
              ,Version [0,5] ["getting there"]
              ,Version [0,6] ["finalized io"]
              ,Version [0,7] ["adding works","creating works"]
              ,Version [0,8] ["removing works","only first and last"]
              ,Version [0,9] ["polish"]
              ]

appNamePretty = "Rmmbr"
appName = "rmmbr"
defaultFile = "todo.txt"
decoLength = 30 
decoChar = '*'
defaultSort = "def"
defaultList = "todo"
usageHelpHeader = "Usage: rmmbr [OPTION...] "
headerLength = 2

commands :: [(String, ((Options, [String]) -> IO (), [OptDescr OptionTransformer], String))]
commands = [ ("show", (present, presentOptions, "view the contents of your todo lists"))
           , ("add", (add, addOptions, "add an entry to one of your todo lists"))
           , ("remove", (remove, removeOptions, "remove an entry from one of your todo lists"))
           , ("create", (create, createOptions, "create a new todo list"))
           , ("help", (help, helpOptions, "show this help information"))
           , ("reset", (reset, resetOptions, "clear all your todo lists, and start over with no entries"))
           ]


data Options = Options { showVerbose :: Bool,
                         showHelp :: Bool,
                         theList :: String,
                         overwrite :: Bool,
                         position :: Position,
                         importance :: String,
                         confirm :: String,
                         sort :: String,
                         interactive :: Bool }

data Position = First | Last

defaultOptions :: Options
defaultOptions = Options { showVerbose = False,
                           showHelp = False,
                           theList = defaultList,
                           overwrite = False,
                           position = Last,
                           importance = "",
                           sort = "",
                           interactive = False }

{-- all the optionTransformer actions are defined here --}

type OptionTransformer = Options -> IO Options

doVersion :: Options -> IO Options
doVersion opts = do putStrLn $ unlines $ map showVersion version_log
                    exitWith ExitSuccess
                    return opts 

doVerbose :: Options -> IO Options
doVerbose opts = do putStrLn "doVerbose" 
                    return opts { showVerbose = True }

doCmdHelp :: [OptDescr OptionTransformer] -> Options -> IO Options
doCmdHelp cmdOpts opts = do putStrLn $ usageInfo usageHelpHeader cmdOpts
                            exitWith ExitSuccess
                            return opts

doHelp :: Options -> IO Options
doHelp opts = do putStrLn "doHelp"
                 return opts { showHelp = True }

doOverwrite :: Options -> IO Options
doOverwrite opts = return opts { overwrite = True }

doRemoveList :: String -> Options -> IO Options
doRemoveList removeName opts = return opts { theList = removeName } 

positions :: [(String, Position)]
positions = [("first", First)
            ,("last", Last)
            ]

doPosition :: String -> Options -> IO Options
doPosition pos opts = do let result = lookup pos positions 
                         case result of
                              Nothing -> do ioError $ userError "The position you provided was invalid"
                                            exitFailure
                                            return opts
                              Just safePos -> return opts { position = safePos }

doAddList :: String -> Options -> IO Options
doAddList list opts = return opts { theList = list }

doImportance :: String -> Options -> IO Options
doImportance level opts = return opts { importance = level }

doShowList :: String -> Options -> IO Options
doShowList list opts = return opts { theList = list }

doSort :: String -> Options -> IO Options
doSort sortBy opts = return opts { sort = sortBy }

doInteractiveRemove :: Options -> IO Options
doInteractiveRemove opts = return opts { interactive = True }

{-- Different commonly occurring options will be defined here so they can be reused --}

commandHelpOption cmdOptions = Option ['h'] ["help"] (NoArg (doCmdHelp cmdOptions)) "shows this message"
versionOption = Option ['V'] ["version"] (NoArg doVersion) "show detailed version information, with changes from previous versions"
verboseOption desc = Option ['v'] ["verbose"] (NoArg doVerbose) desc
programHelpOption = Option ['h'] ["help"] (NoArg doHelp) "show help information for this program"
selectListOption = Option ['l'] ["list"] (ReqArg doShowList "LIST") "choose a list to display"
sortOrderOption = Option ['s'] ["sort"] (ReqArg doSort "SORT ORDER") "specify an order for entries to be sorted in when they are presented"
overwriteOption = Option ['o'] ["overwrite"] (NoArg doOverwrite) "overwrite an already existing list with a new empty list"
positionOption = Option ['p'] ["position"] (ReqArg doPosition "POSITION") "specify an entry by its position in the selected list"
interactiveRemoveOption = Option ['i'] ["interactive"] (NoArg doInteractiveRemove) "interactively remove entries from a list"

{-- the main function --}

main = do putStr $ appNamePretty ++ ": "
          (appDir, _) <- getDirAndLists
          alreadyConfigured <- doesDirectoryExist appDir
          if alreadyConfigured then return () else configure appDir
          input <- getArgs
          let (action,args,options) = if null input then (present, [], []) 
                                                    else case lookup (head input) commands of
                                                         Just (act, opt, _) -> (act, tail input, opt)
                                                         Nothing -> (present, input, presentOptions)
          processedInput <- processInput args options
          action processedInput

{-- Takes the arguments and the list of options and returns the processed options structure and the unrecognized arguments --}

processInput :: [String] -> [OptDescr OptionTransformer] -> IO (Options, [String])
processInput args options = do case getOpt RequireOrder options args of
                                    (optionTransformations, nonOpts, []) -> do opts <- foldl (>>=) (return defaultOptions) optionTransformations
                                                                               return (opts, nonOpts)
                                    (_,_,errors) -> ioError $ userError $ ((unlines errors) ++ (usageInfo usageHelpHeader options))

{-- Shows a list or group of lists --}

presentOptions :: [OptDescr OptionTransformer]
presentOptions = [selectListOption
                 ,verboseOption "show more detailed version of selected lists"
                 ,sortOrderOption
                 ,commandHelpOption presentOptions
                 ,versionOption
                 ]

present :: (Options, [String]) -> IO ()
present (opts,_) = do putStrLn "Showing all entries."
                      let chosenList = theList opts ++ ".txt"
                      (appDir, allLists) <- getDirAndLists
                      if chosenList `elem` allLists then do setCurrentDirectory appDir
                                                            withFile chosenList ReadMode (\handle -> do contents <- hGetContents handle
                                                                                                        putStrLn contents)
                                                    else ioError $ userError $ "The selected list does not exist: " ++ chosenList

{-- Adds one or more entries to a list --}

addOptions :: [OptDescr OptionTransformer]
addOptions = [commandHelpOption addOptions
             ,selectListOption
             ]

add :: (Options, [String]) -> IO ()
add (opts,remaining) = do putStrLn "Adding an entry."
                          let chosenList = theList opts ++ ".txt"
                          (appDir, allLists) <- getDirAndLists              
                          currentTime <- getCurrentTime
                          if chosenList `elem` allLists then do setCurrentDirectory appDir
                                                                withFile chosenList AppendMode (\handle ->
                                                                                                  mapM_ (\entry ->
                                                                                                        do hPutStr handle $ formatTime defaultTimeLocale "%D (%X) - " currentTime
                                                                                                           hPutStrLn handle entry) remaining)
                                                        else ioError $ userError "The selected list does not exist."

{-- Removes one or more entries from a list --}

removeOptions :: [OptDescr OptionTransformer]
removeOptions = [commandHelpOption removeOptions
                ,selectListOption
                ,positionOption
                ]

remove :: (Options, [String]) -> IO ()
remove (opts,remaining) = do putStrLn "Removing an entry."
                             let chosenList = theList opts ++ ".txt"
                                 chosenPosition = position opts
                             (appDir, allLists) <- getDirAndLists
                             if chosenList `elem` allLists then return () else ioError $ userError "The selected list does not exist."
                             tempFile <- writeToTempFile chosenList appDir
                             contents <- readFile tempFile
                             let (header,entries) = splitAt headerLength $ lines contents
                                 (removed,kept) = case chosenPosition of 
                                                       First -> (head entries, tail entries) 
                                                       Last -> (last entries, init entries)
                             putStrLn $ "Removing: " ++ removed
                             handle <- openFile chosenList WriteMode
                             hPutStr handle $ unlines $ header ++ kept
                             hFlush handle
                             hClose handle
                             removeFile tempFile
                             present (opts,remaining)
                             
writeToTempFile :: String -> FilePath -> IO FilePath
writeToTempFile fileName appDir = do (tempName,tempHandle) <- openTempFile appDir fileName
                                     contents <- readFile fileName
                                     hPutStr tempHandle contents
                                     hFlush tempHandle
                                     hClose tempHandle
                                     return tempName

{-- Creates a new list --}           

createOptions :: [OptDescr OptionTransformer]
createOptions = [commandHelpOption createOptions
                ,selectListOption
                ,overwriteOption
                ]

create :: (Options, [String]) -> IO ()
create (opts,remaining) = do putStrLn "Creating a new list of entries."
                             let newList = theList opts ++ ".txt"
                             (appDir, allLists) <- getDirAndLists
                             if (not (newList `elem` allLists)) || (overwrite opts) then do setCurrentDirectory appDir
                                                                                            createList newList
                                                                                            present (opts,remaining)
                                                                                    else ioError $ userError "This list exists already. If you want to replace it with a new blank list, repeat this command with \"-o\"."

createList :: FilePath -> IO ()
createList newList = do handle <- openFile newList WriteMode
                        currentTime <- getCurrentTime
                        hPutStrLn handle $ (replicate decoLength decoChar) ++ " " ++ newList ++ " " ++ (replicate decoLength decoChar) 
                        hPutStrLn handle $ formatTime defaultTimeLocale "Created on %D at: %X" currentTime
                        hClose handle

{-- Show help information --}

helpOptions :: [OptDescr OptionTransformer]
helpOptions = [commandHelpOption helpOptions
              ]

help :: (Options, [String]) -> IO ()
help (opts,_) = do putStrLn "Displaying help for the program."
                   let (cmd, cmdData) = unzip commands 
                       (_, _, helpText) = unzip3 cmdData
                   putStrLn $ inTwoColumns $ zip cmd helpText

{-- Clear all application data --}
                       
resetOptions :: [OptDescr OptionTransformer]
resetOptions = [commandHelpOption resetOptions
               ,overwriteOption
               ]

reset :: (Options, [String]) -> IO ()
reset (opts,_) = do putStrLn "Clearing all lists."
                    let confirm = overwrite opts
                    if confirm then do (_, allLists) <- getDirAndLists
                                       return ()
                               else return ()

configure :: FilePath -> IO ()
configure appDir = do putStrLn "Creating appropriate directories and config files..."
                      createDirectoryIfMissing True appDir
                      setCurrentDirectory appDir
                      createList defaultFile

getDirAndLists :: IO (FilePath, [String])
getDirAndLists = do appDir <- getAppUserDataDirectory appName
                    setCurrentDirectory appDir
                    dirContents <- getDirectoryContents appDir
                    files <- filterM doesFileExist dirContents
                    let lists = filter (".txt" `List.isSuffixOf`) files
                    return (appDir, lists)
