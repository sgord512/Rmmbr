--Filename: rmmbr.hs
--Project: Rmmbr, a command-line program for handling reminders
--Author: Spencer Gordon
--Date: July 22nd, 2011

module Main where

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Locale
import System.Exit
import System.Directory
import Data.Time
import Control.Monad
import qualified Data.List as List
import Data.Maybe( fromMaybe, isNothing )
import Data.Version
import Toolbox( inTwoColumns )

versionLog = [Version [0,3] ["fixed io"]
             ,Version [0,4] ["almost stable"]
             ,Version [0,5] ["getting there"]
             ,Version [0,6] ["finalized io"]
             ,Version [0,7] ["adding works","creating works"]
             ,Version [0,8] ["removing works","only first and last"]
             ,Version [0,9] ["polish"]
             ,Version [1,0] ["first release", "all basic functionality in place and working"]
             ]

appNamePretty = "Rmmbr"
appName = "rmmbr"
decoLength = 30 
decoChar = '*'
defaultSort = "def"
defaultListFile = "todo.txt"
usageHelpHeader = "Usage: rmmbr [OPTION...] "
headerLength = 2

commands :: [(String, ((Options, [String]) -> IO (), [OptDescr OptionTransformer], OptionTransformer, String))]
commands = [ ("show", (present, presentOptions, assumeLists, "view the contents of your todo lists"))
           , ("add", (add, addOptions, (\opts -> return opts { nonOptsHandler = doEntry }), "add an entry to one of your todo lists"))
           , ("remove", (remove, removeOptions, (\opts -> return opts { nonOptsHandler = doPosition }), "remove an entry from one of your todo lists"))
           , ("create", (create, createOptions, assumeSelectList, "create a new todo list"))
           , ("help", (help, helpOptions, return, "show this help information"))
           , ("reset", (reset, resetOptions, return, "clear all your todo lists, and start over with no entries"))
           ]

assumeLists :: OptionTransformer
assumeLists opts = return opts { nonOptsHandler = doShowList }

assumeSelectList :: OptionTransformer
assumeSelectList opts = return opts { nonOptsHandler = doSelectList }

throwUserError :: String -> OptionTransformer
throwUserError str = (\opts -> ioError $ userError $ "Couldn't parse the following argument: " ++ str)

data Options = Options { nonOptsHandler :: String -> OptionTransformer,
                         showVerbose :: Bool,
                         showHelp :: Bool,
                         overwrite :: Bool,
--                         interactive :: Bool,
                         position :: Either Position [Position],
                         theShowList :: Either String [String],
                         theList :: String,
                         entries :: [String],
                         importance :: String,
                         confirm :: String,
                         sort :: String }

data Position = First | Last

defaultOptions :: Options
defaultOptions = Options { nonOptsHandler = throwUserError,
                           showVerbose = False,
                           showHelp = False,
                           overwrite = False,
--                           interactive = False,
                           position = Left Last,
                           theShowList = Left defaultListFile,
                           theList = defaultListFile,
                           entries = [],
                           importance = "",
                           sort = "" }


{-- all the optionTransformer actions are defined here --}

type OptionTransformer = Options -> IO Options

doVersion :: OptionTransformer
doVersion opts = do putStrLn $ unlines $ map showVersion versionLog
                    exitWith ExitSuccess
                    return opts 

doVerbose :: OptionTransformer
doVerbose opts = do putStrLn "doVerbose" 
                    return opts { showVerbose = True }

doCmdHelp :: [OptDescr OptionTransformer] -> OptionTransformer
doCmdHelp cmdOpts opts = do putStrLn $ usageInfo usageHelpHeader cmdOpts
                            exitWith ExitSuccess
                            return opts

doHelp :: OptionTransformer
doHelp opts = do putStrLn "doHelp"
                 return opts { showHelp = True }

doOverwrite :: OptionTransformer
doOverwrite opts = return opts { overwrite = True }

doRemoveList :: String -> OptionTransformer
doRemoveList removeName opts = return opts { theList = removeName ++ ".txt" } 

doEntry :: String -> OptionTransformer
doEntry entry opts = return opts { entries = entry:(entries opts) }

positions :: [(String, Position)]
positions = [("first", First)
            ,("last", Last)
            ]

doPosition :: String -> OptionTransformer
doPosition pos opts = do let result = lookup pos positions 
                         case result of
                              Nothing -> do ioError $ userError "The position you provided was invalid"
                                            exitFailure
                                            return opts
                              Just safePos -> case position opts of
                                                   Left _ -> return opts { position = Right [safePos], nonOptsHandler = throwUserError }
                                                   Right positions -> return opts { position = Right (safePos:positions) }

doSelectList :: String -> OptionTransformer
doSelectList list opts = return opts { theList = list ++ ".txt", nonOptsHandler = doEntry }

doImportance :: String -> OptionTransformer
doImportance level opts = return opts { importance = level }

doShowList :: String -> OptionTransformer
doShowList list opts = return opts { theShowList = case theShowList opts of
                                                        Left _ -> Right [list ++ ".txt"]
                                                        Right lists -> Right ((list ++ ".txt"):lists) }

--doSort :: String -> OptionTransformer
--doSort sortBy opts = return opts { sort = sortBy }

--doInteractiveRemove :: OptionTransformer
--doInteractiveRemove opts = return opts { interactive = True }

doAllLists :: OptionTransformer
doAllLists opts = do (_, allLists) <- getDirAndLists                     
                     foldl (>>=) (return opts {nonOptsHandler = throwUserError} ) (map (doShowList . takeWhile (\char -> not (char == '.'))) allLists)                      

doNonOptsHandling :: String -> OptionTransformer
doNonOptsHandling str opts = do (nonOptsHandler opts) str opts


{-- Different commonly occurring options will be defined here so they can be reused --}

commandHelpOption cmdOptions = Option ['h'] ["help"] (NoArg (doCmdHelp cmdOptions)) "shows this message"
versionOption = Option ['V'] ["version"] (NoArg doVersion) "show detailed version information, with changes from previous versions"
verboseOption desc = Option ['v'] ["verbose"] (NoArg doVerbose) desc
programHelpOption = Option ['h'] ["help"] (NoArg doHelp) "show help information for this program"
selectListOption = Option ['l'] ["list"] (ReqArg doSelectList "LIST") "select a list to be the target of your command"
showListOption = Option ['l'] ["list"] (ReqArg doShowList "LIST") "specify which lists to display"
--sortOrderOption = Option ['s'] ["sort"] (ReqArg doSort "SORT ORDER") "specify an order for entries to be sorted in when they are presented"
overwriteOption = Option ['o'] ["overwrite"] (NoArg doOverwrite) "overwrite an already existing list with a new empty list"
positionOption = Option ['p'] ["position"] (ReqArg doPosition "POSITION") "specify an entry by its position in the selected list"
--interactiveRemoveOption = Option ['i'] ["interactive"] (NoArg doInteractiveRemove) "interactively remove entries from a list"
allListsOption = Option ['a'] ["all"] (NoArg doAllLists) "show all lists"


{-- the main function --}

main = do putStr $ appNamePretty ++ ": "
          (appDir, _) <- getDirAndLists
          alreadyConfigured <- doesDirectoryExist appDir
          unless alreadyConfigured $ configure appDir
          input <- getArgs
          let (action,args, optsSetup, options) = if null input then (present, [], return, []) 
                                                                else case lookup (head input) commands of
                                                                     Just (act, opt, optsSetup, _) -> (act, tail input, optsSetup, opt)
                                                                     Nothing -> (present, input, assumeLists, presentOptions)
          processedInput <- processInput args options optsSetup
          action processedInput

{-- Takes the arguments and the list of options and returns the processed options structure and the unrecognized arguments --}

processInput :: [String] -> [OptDescr OptionTransformer] -> OptionTransformer -> IO (Options, [String])
processInput args options optSetup = case getOpt (ReturnInOrder doNonOptsHandling) options args of
                                                 (optionTransformations, nonOpts, []) -> do opts <- foldl (>>=) (optSetup defaultOptions) optionTransformations
                                                                                            return (opts, nonOpts)
                                                 (_,_,errors) -> ioError $ userError $ ((unlines errors) ++ (usageInfo usageHelpHeader options))

{-- Shows a list or group of lists --}

presentOptions :: [OptDescr OptionTransformer]
presentOptions = [showListOption
                 ,verboseOption "show more detailed version of selected lists"
--                 ,sortOrderOption
                 ,commandHelpOption presentOptions
                 ,versionOption
                 ,allListsOption
                 ]

present :: (Options, [String]) -> IO ()
present (opts,remaining) = do putStrLn "Showing all entries."
                              chosenList <- case theShowList opts of
                                            Left list -> return list
                                            Right (list:[]) -> return list
                                            Right (list:lists) -> do present $ (opts {theShowList = Right lists}, remaining)
                                                                     return list
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
                          let chosenList = theList opts 
                          (appDir, allLists) <- getDirAndLists              
                          currentTime <- getCurrentTime
                          if chosenList `elem` allLists then do setCurrentDirectory appDir
                                                                withFile chosenList AppendMode (\handle ->
                                                                                                  mapM_ (\entry ->
                                                                                                        do hPutStr handle $ formatTime defaultTimeLocale "%D (%X) - " currentTime
                                                                                                           hPutStrLn handle entry) (entries opts))
                                                        else ioError $ userError "The selected list does not exist."

{-- Removes one or more entries from a list --}

removeOptions :: [OptDescr OptionTransformer]
removeOptions = [commandHelpOption removeOptions
                ,selectListOption
                ,positionOption
                ]

remove :: (Options, [String]) -> IO ()
remove (opts,remaining) = do putStrLn "Removing an entry."
                             let chosenList = theList opts 
                                 chosenPosition = position opts
                             (appDir, allLists) <- getDirAndLists
                             unless (chosenList `elem` allLists) $ ioError $ userError "The selected list does not exist."
                             tempFile <- writeToTempFile chosenList appDir
                             contents <- readFile tempFile
                             let (header,entries) = splitAt headerLength $ lines contents
                                 (removed,kept) = case chosenPosition of 
                                                       Left Last -> (head entries, tail entries) 
                                                       Right (Last:others) -> (last entries, init entries)
                                                       Right (First:others) -> (head entries, tail entries)
                             putStrLn $ "Removing: " ++ removed
                             writeFile chosenList $ unlines $ header ++ kept
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
                             let newList = theList opts 
                             (appDir, allLists) <- getDirAndLists
                             if (newList `notElem` allLists) || (overwrite opts) then do setCurrentDirectory appDir
                                                                                         createList newList
                                                                                         present (opts { theShowList = Left (theList opts) } ,remaining)
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
                       (_, _, _, helpText) = List.unzip4 cmdData
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
                                       mapM_ removeFile allLists
                                       createList defaultListFile
                                       return ()
                               else do putStrLn "This will remove all your lists permanently. This operation cannot be undone."
                                       noReset <- prompt
                                       unless noReset $ do (_, allLists) <- getDirAndLists
                                                           mapM_ removeFile allLists
                                                           createList defaultListFile
                                                           return ()

prompt :: IO Bool
prompt = do putStr "Do you want to proceed (y / n)? "
            hFlush stdout
            response <- getChar
            case response of
                 'y' -> do putStrLn "Clearing all lists." 
                           return True
                 'n' -> do putStrLn "Ending program." 
                           return False
                 otherwise -> do putStrLn "Invalid response. Please try again."
                                 answer <- prompt
                                 return answer

configure :: FilePath -> IO ()
configure appDir = do putStrLn "Creating appropriate directories and deault list files..."
                      createDirectoryIfMissing True appDir
                      setCurrentDirectory appDir
                      createList defaultListFile

getDirAndLists :: IO (FilePath, [String])
getDirAndLists = do appDir <- getAppUserDataDirectory appName
                    setCurrentDirectory appDir
                    dirContents <- getDirectoryContents appDir
                    files <- filterM doesFileExist dirContents
                    let lists = filter (".txt" `List.isSuffixOf`) files
                    return (appDir, lists)