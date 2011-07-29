--Filename: rmmbr.hs
--Project: Rmmbr, a command-line program for handling reminders
--Author: Spencer Gordon
--Date: July 26th, 2011

module Main where

import Control.Monad
import Data.Array
import Data.List( isSuffixOf, partition, unzip4, zipWith4 )
import Data.Maybe( fromMaybe, isNothing )
import Data.Time
import Data.Version
import System.Console.ANSI
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Locale
import Text.Parsec.Prim

import Toolbox( inTwoColumns )
import Util

versionLog = [Version [0,3] ["fixed io"]
             ,Version [0,4] ["almost stable"]
             ,Version [0,5] ["getting there"]
             ,Version [0,6] ["finalized io"]
             ,Version [0,7] ["adding works","creating works"]
             ,Version [0,8] ["removing works","only first and last"]
             ,Version [0,9] ["polish"]
             ,Version [1,0] ["first release", "all basic functionality in place and working"]
             ,Version [1,1] ["THE ACTUAL FIRST RELEASE", "REMOVE WORKS LIKE MAGIC"]
             ]

appNamePretty = "Rmmbr"


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


{-- all the OptionTransformer actions are defined here --}

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

doPosition :: String -> OptionTransformer
doPosition str opts = case (parse positionArg "position arguments" str) of
                      Left err -> ioError $ userError $ show err
                      Right pos -> return opts { position = case position opts of
                                                                 Left _ -> Right [pos]
                                                                 Right ps -> Right (pos:ps) }

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

main = do --putStr $ appNamePretty ++ ": "
          (appDir, _) <- getDirAndLists
          alreadyConfigured <- doesDirectoryExist appDir
          unless alreadyConfigured $ configure
          input <- getArgs
          let (action,args, optsSetup, options) = if null input then (present, [], return, []) 
                                                                else case lookup (head input) commands of
                                                                     Just (act, opt, optsSetup, _) -> (act, tail input, optsSetup, opt)
                                                                     Nothing -> (present, input, assumeLists, presentOptions)
          processedInput <- processInput args options optsSetup
          action processedInput


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
present (opts,remaining) = do --putStrLn "Showing all entries."
                              (chosenList, newOpts) <- case theShowList opts of
                                                            Left list -> return (list, opts { theShowList = Right [] })
                                                            Right list -> return ((last list), opts {theShowList = Right (init list)})
                              (appDir, allLists) <- getDirAndLists
                              if chosenList `elem` allLists then do setCurrentDirectory appDir
                                                                    withFile chosenList ReadMode (\handle -> do contents <- hGetContents handle
                                                                                                                putStrLn contents)
                                                            else ioError $ userError $ "The selected list does not exist: " ++ chosenList
                              unless (theShowList newOpts == Right []) $ present (newOpts, remaining)
                                                                       

{-- Adds one or more entries to a list --}

addOptions :: [OptDescr OptionTransformer]
addOptions = [commandHelpOption addOptions
             ,selectListOption
             ]

add :: (Options, [String]) -> IO ()
add (opts,remaining) = do --putStrLn "Adding an entry."
                          let chosenList = theList opts
                              entries' = reverse $ entries opts
                          (appDir, allLists) <- getDirAndLists              
                          currentTime <- getCurrentTime
                          if chosenList `elem` allLists then do setCurrentDirectory appDir
                                                                withFile chosenList AppendMode (\handle ->
                                                                                                  mapM_ (\entry ->
                                                                                                        do hPutStr handle $ formatTime defaultTimeLocale "%D (%X) - " currentTime
                                                                                                           hPutStrLn handle entry) entries')
                                                        else ioError $ userError "The selected list does not exist."

{-- Removes one or more entries from a list --}

removeOptions :: [OptDescr OptionTransformer]
removeOptions = [commandHelpOption removeOptions
                ,selectListOption
                ,positionOption
                ]

remove :: (Options, [String]) -> IO ()
remove (opts,remaining) = do --putStrLn "Removing an entry."
                             let chosenList = theList opts 
                                 positions = case position opts of 
                                                  Left pos -> [pos]
                                                  Right ps -> ps
                             (appDir, allLists) <- getDirAndLists
                             unless (chosenList `elem` allLists) $ ioError $ userError "The selected list does not exist."
                             (header,entries) <- getHeaderAndEntries chosenList
                             putStrLn "Showing list:"
                             sequence_ $ zipWith (>>) (map (\num -> (putStr `withColor` Cyan) (show num ++ " ")) [1,2..]) (map putStrLn entries)
                             putStrLn "End of shown list."
                             let arr = listArray (1, length entries) (zip (repeat False) entries)                             
                                 (removed',kept') = partition (\(rm, _) -> rm) (elems (foldl removePositionFromList arr positions))
                                 (removed, kept) = ((snd $ unzip removed'), (snd $ unzip kept'))
                             sequence_ $ zipWith (>>) (map (\x -> putStr "Removing: ") [1,2..]) (map (\r -> (putStrLn `withColor` Red) r) removed)
                             writeFile chosenList $ unlines $ header ++ kept
                             present (opts,remaining)
{-- Creates a new list --}           

createOptions :: [OptDescr OptionTransformer]
createOptions = [commandHelpOption createOptions
                ,selectListOption
                ,overwriteOption
                ]

create :: (Options, [String]) -> IO ()
create (opts,remaining) = do --putStrLn "Creating a new list of entries."
                             let newList = theList opts 
                             (appDir, allLists) <- getDirAndLists
                             if (newList `notElem` allLists) || (overwrite opts) then do setCurrentDirectory appDir
                                                                                         createList newList
                                                                                         present (opts { theShowList = Left (theList opts) } ,remaining)
                                                                                 else ioError $ userError "This list exists already. If you want to replace it with a new blank list, repeat this command with \"-o\"."

{-- Show help information --}

helpOptions :: [OptDescr OptionTransformer]
helpOptions = [commandHelpOption helpOptions
              ]

help :: (Options, [String]) -> IO ()
help (opts,_) = do --putStrLn "Displaying help for the program."
                   let (cmd, cmdData) = unzip commands 
                       (_, _, _, helpText) = unzip4 cmdData
                   putStrLn $ inTwoColumns $ zip cmd helpText

{-- Clear all application data --}
                       
resetOptions :: [OptDescr OptionTransformer]
resetOptions = [commandHelpOption resetOptions
               ,overwriteOption
                              ]

reset :: (Options, [String]) -> IO ()
reset (opts,_) = do --putStrLn "Clearing all lists."
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

