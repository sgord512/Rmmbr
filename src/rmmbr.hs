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

appNamePretty = "Rmmbr"
appName = "rmmbr"
configFilePath = "rmmbr.config"
defaultFile = "todo.txt"
decoLength = 30 
defaultSort = "def"
defaultList = "todo"

commands :: [(String, ([String] -> [OptDescr Flags] -> IO (), [OptDescr Flags]))]
commands = [ ("show", (present, presentOptions))
           , ("add", (add, addOptions))
           , ("remove", (remove, removeOptions))
           , ("create", (create, createOptions))
           , ("help", (help, helpOptions))
           , ("reset", (reset, resetOptions))
           ]

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

{-- Different commonly occurring options will be defined here so they can be reused --}

commandHelpOption = Option ['h'] ["help"] (NoArg Help) "get help for this command"

-- OptDescr a = Option [Char] [String] (ArgDescr a) String
mainOptions :: [OptDescr Flags]
mainOptions = [Option ['v'] ["verbose"] (NoArg Verbose) "provide more detailed output"
              ,Option ['V'] ["version"] (NoArg Version) "show detailed version information, with changes from previous versions"
              ,Option ['h'] ["help"] (NoArg Help) "show help information for this program"
              ]


main = do putStr $ appNamePretty ++ ": "
          appDir <- getAppUserDataDirectory appName
          alreadyConfigured <- doesDirectoryExist appDir
          if alreadyConfigured then return () else configure appDir
          input <- getArgs
          if null input then do let (action, options) = (present, presentOptions)
                                let args = []
                                action args options
                                   
                        else do let (command:args) = input
                                let (action, options) = case lookup command commands of
                                                      Just value -> value
                                                      Nothing -> (help, helpOptions)
                                action args options

presentOptions :: [OptDescr Flags]
presentOptions = [Option ['l'] ["list"] (OptArg makeShowList "LIST") "choose a list to display"
                 ,Option ['v'] ["verbose"] (NoArg Verbose) "show more detailed version of selected lists"
                 ,Option ['s'] ["sort"] (OptArg makeSort "SORT ORDER") "specify an order for entries to be sorted in when they are presented"
                 ,commandHelpOption]

present :: [String] -> [OptDescr Flags] -> IO ()
present args options = do let (flags, nonOpts, msgs) = getOpt RequireOrder options args
                          putStrLn "Showing all entries."

addOptions :: [OptDescr Flags]
addOptions = [commandHelpOption]

add :: [String] -> [OptDescr Flags] -> IO ()
add args options = do let (flags, nonOpts, msgs) = getOpt RequireOrder options args 
                      putStrLn "Adding an entry."

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

removeOptions :: [OptDescr Flags]
removeOptions = [commandHelpOption
                ]

remove :: [String] -> [OptDescr Flags] -> IO ()
remove args options = do let (flags, nonOpts, msgs) = getOpt RequireOrder options args 
                         putStrLn "Removing an entry."

createOptions :: [OptDescr Flags]
createOptions = [commandHelpOption
                ]

create :: [String] -> [OptDescr Flags] -> IO ()
create args options = do let (flags, nonOpts, msgs) = getOpt RequireOrder options args 
                         putStrLn "Creating a list of entries."

helpOptions :: [OptDescr Flags]
helpOptions = [commandHelpOption
              ]

help :: [String] -> [OptDescr Flags] -> IO ()
help args options = do let (flags, nonOpts, msgs) = getOpt RequireOrder options args 
                       putStrLn "Displaying help for the program."

resetOptions :: [OptDescr Flags]
resetOptions = []

reset :: [String] -> [OptDescr Flags] -> IO ()
reset args options = do let (flags, nonOpts, msgs) = getOpt RequireOrder options args
                        putStrLn "Displaying help for the reset command"

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
