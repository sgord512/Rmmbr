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

import Toolbox( inTwoColumns, padFrontUntilLength, withColor )
import Util
import Entry

versionLog = [Version [0,3] ["fixed io"]
             ,Version [0,4] ["almost stable"]
             ,Version [0,5] ["getting there"]
             ,Version [0,6] ["finalized io"]
             ,Version [0,7] ["adding works","creating works"]
             ,Version [0,8] ["removing works","only first and last"]
             ,Version [0,9] ["polish"]
             ,Version [1,0] ["first release", "all basic functionality in place and working"]
             ,Version [1,1] ["THE ACTUAL FIRST RELEASE", "REMOVE WORKS LIKE MAGIC"]
             ,Version [1,2] ["importance, completion, comment", "more formatting"]
             ,Version [1,3] ["integrated advanced entries", "refactoring and documentation"]
             ,Version [1,4] ["comments can now be controlled via the command line", "entries can now be controlled via the command line"]
             ,Version [1,5] ["sorting works", "cleanup implemented"]
             ]
             
appNamePretty = "Rmmbr"


commands :: [(String, ((Options, [String]) -> IO (), [OptDescr OptionTransformer], OptionTransformer, String))]
commands = [ ("show", (present, presentOptions, assumeLists, "view the contents of your todo lists"))
           , ("add", (add, addOptions, (\opts -> return opts { nonOptsHandler = doEntries }), "add an entry to one of your todo lists"))
           , ("remove", (remove, removeOptions, (\opts -> return opts { nonOptsHandler = doPositions }), "remove an entry from one of your todo lists"))
           , ("create", (create, createOptions, (\opts -> return opts { nonOptsHandler = doCreateList }), "create a new todo list"))
           , ("help", (help, helpOptions, (\opts -> return opts { nonOptsHandler = doHelpForCommand }), "show this help information"))
           , ("reset", (reset, resetOptions, return, "clear all your todo lists, and start over with no entries"))
           , ("comment", (commentEntry, commentOptions, (\opts -> return opts { nonOptsHandler = doComment }), "set the comment for an entry, or append a comment to an entry"))
--           , ("comment-list", (comment_list, commentListOptions, (\opts -> return opts { nonOptsHandler = doComment }), "set the comment for a list, or append a comment to a list"))  
           , ("done", (complete_entry Complete, completeEntryOptions, (\opts -> return opts {nonOptsHandler = doPositions }), "check an entry off your list as complete"))
           , ("begin", (complete_entry InProgress, completeEntryOptions, (\opts -> return opts { nonOptsHandler = doPositions }), "mark an entry as in progress"))
           , ("cleanup", (cleanup, cleanupOptions, return, "take completed items off a list"))
--           , ("edit", (edit, editOptions, return, "interactively edit your lists"))
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

doHelpForCommand :: String -> OptionTransformer
doHelpForCommand cmd opts = do return opts { helpCmd = (Just cmd), nonOptsHandler = throwUserError }

doOverwrite :: OptionTransformer
doOverwrite opts = return opts { overwrite = True }

doRemoveList :: String -> OptionTransformer
doRemoveList removeName opts = return opts { theList = removeName ++ ".txt" }

doEntries :: String -> OptionTransformer
doEntries title opts = do utcTime <- getCurrentTime
                          let entry = (Entry (MakeAddTime utcTime) (MakeTitle title) NotStarted Medium (MakeComment "(default comment)"))
                          return opts { entries = entry:(entries opts) }

doComment :: String -> OptionTransformer
doComment str opts = return opts { comment = str, nonOptsHandler = throwUserError }

doPositions :: String -> OptionTransformer
doPositions str opts = case (parse positionArg "position arguments" str) of
                       Left err -> inputError $ show err
                       Right pos -> return opts { position = case position opts of
                                                                  Left _ -> Right [pos]
                                                                  Right ps -> Right (pos:ps) }

doPosition :: String -> OptionTransformer
doPosition str opts = case (parse positionArg "postions argument" str) of
                       Left err -> inputError $ show err
                       Right pos -> return opts { position = Left pos }
                                                                  

doSelectList :: String -> OptionTransformer
doSelectList list opts = do (appDir, allLists) <- getDirAndLists
                            validateList list allLists
                            return opts { theList = list ++ ".txt", nonOptsHandler = doEntries }

doCreateList :: String -> OptionTransformer
doCreateList list opts = return opts { theList = list ++ ".txt", nonOptsHandler = throwUserError }

doImportance :: String -> OptionTransformer
doImportance level opts = return opts { importance = level }

doShowList :: String -> OptionTransformer
doShowList list opts = do (appDir, allLists) <- getDirAndLists
                          validateList list allLists
                          return opts { theShowList = case theShowList opts of
                                                           Left _ -> Right [list ++ ".txt"]
                                                           Right lists -> Right ((list ++ ".txt"):lists) }




doSort :: String -> OptionTransformer
doSort sortBy opts = do case (parse sortArg "sorting method argument" sortBy) of
                             Left err -> inputError $ show err
                             Right sortFunction -> return opts { sort = sortFunction }

--doInteractiveRemove :: OptionTransformer
--doInteractiveRemove opts = return opts { interactive = True }

doAllLists :: OptionTransformer
doAllLists opts = do (_, allLists) <- getDirAndLists
                     foldl (>>=) (return opts {nonOptsHandler = throwUserError} ) (map (doShowList . takeWhile (\char -> not (char == '.'))) allLists)


{-- Different commonly occurring options will be defined here so they can be reused --}

commandHelpOption cmdOptions = Option ['h'] ["help"] (NoArg (doCmdHelp cmdOptions)) "shows this message"
versionOption = Option ['V'] ["version"] (NoArg doVersion) "show detailed version information, with changes from previous versions"
verboseOption desc = Option ['v'] ["verbose"] (NoArg doVerbose) desc
selectListOption = Option ['l'] ["list"] (ReqArg doSelectList "LIST") "select a list to be the target of your command"
showListOption = Option ['l'] ["list"] (ReqArg doShowList "LIST") "specify which lists to display"
sortOrderOption = Option ['s'] ["sort"] (ReqArg doSort "SORT ORDER") "specify an order for entries to be sorted in when they are presented"
overwriteOption = Option ['o'] ["overwrite"] (NoArg doOverwrite) "overwrite an already existing list with a new empty list"
positionsOption = Option ['p'] ["position"] (ReqArg doPositions "POSITION") "specify an entry by its position in the selected list"
--interactiveRemoveOption = Option ['i'] ["interactive"] (NoArg doInteractiveRemove) "interactively remove entries from a list"
allListsOption = Option ['a'] ["all"] (NoArg doAllLists) "show all lists"
positionOption = Option ['p'] ["position"] (ReqArg doPosition "POSITION") "specify an entry by its position in the selected list"


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
                 ,sortOrderOption
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
                              setCurrentDirectory appDir
                              (header, entries) <- getHeaderAndEntries chosenList (sort opts)
                              let sep = words (head header)
                                  title = (putStr $ (head sep) ++ " "):((putStr `withColor` Yellow) $ sep !! 1):(putStrLn $ " " ++ (last sep)):(putStrLn (last header)):[]
                              sequence_ title
                              let maxDigits = length $ show $ length entries
                              sequence_ $ zipWith (>>) (map (\num -> (putStr `withColor` Magenta) ((padFrontUntilLength ' ' maxDigits (show num)) ++ " ")) [1,2..]) (map colorPrint entries)
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
                          setCurrentDirectory appDir
                          withFile chosenList AppendMode (\handle -> mapM_ (\entry -> hPutStrLn handle (show entry)) entries')                           

{-- Removes one or more entries from a list --}

removeOptions :: [OptDescr OptionTransformer]
removeOptions = [commandHelpOption removeOptions
                ,selectListOption
                ,positionsOption
                ,sortOrderOption
                ]

remove :: (Options, [String]) -> IO ()
remove (opts,remaining) = do --putStrLn "Removing an entry."
                             let chosenList = theList opts
                                 positions = case position opts of
                                                  Left pos -> [pos]
                                                  Right ps -> ps
                             (appDir, allLists) <- getDirAndLists
                             (header,entries) <- getHeaderAndEntries chosenList (sort opts)
                             putStrLn "Showing list:"
                             let maxDigits = length $ show $ length entries
                             sequence_ $ zipWith (>>) (map (\num -> (putStr `withColor` Cyan) ((padFrontUntilLength ' ' maxDigits (show num)) ++ " ")) [1,2..]) (map (putStrLn . show) entries)
                             putStrLn "End of shown list."
                             let arr = listArray (1, length entries) (zip (repeat False) entries)
                                 (removed',kept') = partition (\(rm, _) -> rm) (elems (foldl removePositionFromList arr positions))
                                 (removed, kept) = ((snd $ unzip removed'), (snd $ unzip kept'))
                             sequence_ $ zipWith (>>) (map (\x -> putStr "Removing: ") [1,2..]) (map (\r -> (putStrLn `withColor` Red) (show r)) removed)
                             writeFile chosenList $ unlines $ header ++ (map show kept)
                             present (opts,remaining)
{-- Creates a new list --}

createOptions :: [OptDescr OptionTransformer]
createOptions = [commandHelpOption createOptions
                ,selectListOption
                ,overwriteOption
                ,sortOrderOption
                ]

create :: (Options, [String]) -> IO ()
create (opts,remaining) = do --putStrLn "Creating a new list of entries."
                             let newList = theList opts
                             (appDir, allLists) <- getDirAndLists
                             if (newList `notElem` allLists) || (overwrite opts) then do setCurrentDirectory appDir
                                                                                         createList newList
                                                                                         present (opts { theShowList = Left (theList opts) } ,remaining)
                                                                                 else inputError "This list exists already. If you want to replace it with a new blank list, repeat this command with \"-o\"."

{-- Show help information --}

helpOptions :: [OptDescr OptionTransformer]
helpOptions = [commandHelpOption helpOptions
              ]

help :: (Options, [String]) -> IO ()
help (opts,_) = do --putStrLn "Displaying help for the program."
                   case helpCmd opts of 
                        Nothing -> do let (cmd, cmdData) = unzip commands
                                          (_, _, _, helpText) = unzip4 cmdData
                                      putStrLn $ inTwoColumns $ zip cmd helpText
                        Just cmd -> do case lookup cmd commands of
                                            Nothing -> do putStrLn "Command not recognized. For a list of commands, try \"help\" with no arguments."
                                                          exitFailure
                                            Just (_, cmdOpts, _, helpText) -> do putStrLn $ inTwoColumns $ zip [cmd] [helpText]
                                                                                 doCmdHelp cmdOpts opts
                                                                                 exitSuccess

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

{-- Add a comment to an entry, or change the current comment --}

commentOptions :: [OptDescr OptionTransformer]
commentOptions = [commandHelpOption commentOptions
                 ,overwriteOption
                 ,selectListOption
                 ,positionOption
                 ,sortOrderOption
                 ]

commentEntry :: (Options, [String]) -> IO ()
commentEntry (opts,_) = do --putStrLn "Adding comment to entry"
                           let chosenList = theList opts
                               (Left pos) = position opts
                               cmt = comment opts
                           (appDir, allLists) <- getDirAndLists
                           (header, entries) <- getHeaderAndEntries chosenList (sort opts)
                           let arr = listArray (1, length entries) entries
                               action = (if overwrite opts then overwriteComment else appendComment) cmt
                               update = (\i a -> a//[(i, action (a!i))])
                               entries' = elems $ applyToPosition pos update arr
                           writeFile chosenList $ unlines $ header ++ (map show entries')
                      
{-- Add a comment to a list, or change the current comment --}
 
commentListOptions :: [OptDescr OptionTransformer]
commentListOptions = [commandHelpOption commentOptions
                     ,overwriteOption
                     ]


comment_list :: (Options, [String]) -> IO ()
comment_list (opts,_) = do --putStrLn "Adding comment to list"
                           return ()


{-- Change the completion status of entries --}

completeEntryOptions :: [OptDescr OptionTransformer]
completeEntryOptions = [commandHelpOption completeEntryOptions
                       ,selectListOption
                       ,positionsOption
                       ,sortOrderOption
                       ]

complete_entry :: CompletionStatus -> (Options, [String]) -> IO ()
complete_entry comp (opts,_) = do --putStrLn "Changing the completion status of entries"
                                  let chosenList = theList opts
                                      positions = case position opts of
                                                       Left pos -> [pos]
                                                       Right pos -> pos
                                  (appDir, allLists) <- getDirAndLists
                                  (header, entries) <- getHeaderAndEntries chosenList (sort opts)
                                  let arr = listArray (1, length entries) entries
                                      update = (\i a -> a//[(i, (changeCompStatus comp) (a!i))])
                                      entries' = elems $ foldl (\a p -> applyToPosition p update a) arr positions
                                  writeFile chosenList $ unlines $ header ++ (map show entries')

cleanupOptions :: [OptDescr OptionTransformer]
cleanupOptions = [commandHelpOption cleanupOptions
                 ,selectListOption
                 ]

cleanup :: (Options, [String]) -> IO ()
cleanup (opts, _) = do --putStrLn "Removing all completed entries"
                       let chosenList = theList opts
                       (appDir, allLists) <- getDirAndLists
                       (header, entries) <- getHeaderAndEntries chosenList (sort opts)
                       let (removed', kept') = partition (\(Entry _ _ compStatus _ _) -> compStatus == Complete) entries
                       writeFile chosenList $ unlines $ header ++ (map show kept')