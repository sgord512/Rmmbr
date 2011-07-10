module Main where

import System.Environment
import System.IO
import System.Locale
import System.Exit
import System.Directory
import Data.Time

appNamePretty = "Rmmbr"
appName = "rmmbr"
configFile = "rmmbr.config"
defaultFile = "todo.txt"
decoLength = 30

main = do 
     putStr $ appNamePretty ++ ": "
     args <- getArgs
     appDir <- getAppUserDataDirectory appName
     alreadyConfigured <- doesDirectoryExist appDir
     if alreadyConfigured then return () else configure appDir
     setCurrentDirectory appDir
     fileLocations <- getFileFromConfig configFile
     processArgs fileLocations args                        
     exitSuccess

addItems :: [String] -> Handle -> IO ()
addItems [] file = return ()
addItems (x:xs) file = do putStrLn ("adding " ++ x)
                          currentTime <- getCurrentTime
                          hPutStr file $ formatTime defaultTimeLocale "\n%D (%X) - " currentTime 
                          hPutStrLn file x
                          addItems xs file

processArgs :: [FilePath] -> [String] -> IO ()
processArgs fileLocations [] = do putStrLn "Showing all recorded entries."
                                  itemsList <- readFile (head fileLocations)
                                  putStrLn (replicate decoLength '-')
                                  putStrLn itemsList
                                  putStrLn (replicate decoLength '-')
processArgs fileLocations args = do putStrLn "Adding a new entry."
                                    listFile <- openFile (head fileLocations) AppendMode
                                    addItems args listFile 
                                    hClose listFile

configure :: FilePath -> IO ()
configure appDir = do putStrLn "Creating appropriate directories and config files..."
                      createDirectoryIfMissing True appDir
                      setCurrentDirectory appDir
                      writeFile configFile ""
                      writeFile defaultFile ((replicate decoLength '*') ++ "\ntodo.txt\n" ++ (replicate decoLength '*'))
                      
getFileFromConfig :: FilePath -> IO [FilePath]
getFileFromConfig filePath = do handle <- openFile filePath ReadMode
                                doneReading <- hIsEOF handle
                                if not doneReading then readFromConfig handle
                                                   else return $ repeat defaultFile
readFromConfig :: Handle -> IO [String]
readFromConfig handle =  do line <- hGetLine handle 
                            if (head (words line)) == "FILE" then return $ repeat ((words line) !! 2)
                                                             else return $ repeat defaultFile
