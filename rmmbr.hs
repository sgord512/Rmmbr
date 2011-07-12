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

appNamePretty = "Rmmbr"
appName = "rmmbr"
configFilePath = "rmmbr.config"
defaultFile = "todo.txt"
decoLength = 30 

commands :: [(String, [String] -> IO () ) ]
commands = [ ("show", present)
           , ("add", add)
           , ("remove", remove)
           , ("create", create)
           , ("help", help)
           ]

main = do putStr $ appNamePretty ++ ": "
          appDir <- getAppUserDataDirectory appName
          alreadyConfigured <- doesDirectoryExist appDir
          if alreadyConfigured then return () else configure appDir
          (command:args) <- getArgs
          let (Just action) = case lookup command commands of
                                   Just this -> this
                                   Nothing -> show
          action args
          exitSuccess

present :: [String] -> IO ()
present _ = do putStrLn "Showing all entries."

add :: [String] -> IO ()
add _ = do putStrLn "Adding an entry."

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


remove :: [String] -> IO ()
remove _ = do putStrLn "Removing an entry."

create :: [String] -> IO ()
create _ = do putStrLn "Creating a list of entries."

help :: [String] -> IO ()
help _ = do putStrLn "Displaying help for the program."

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
                                              return head:rest
                                      else return (Just defaultFile):[]

readLineFromConfig :: Handle -> IO (Maybe FilePath)
readLineFromConfig handle =  do line <- hGetLine handle 
                                if (head (words line)) == "FILE" then return (Just ((words line) !! 2))
                                                                 else return Nothing
