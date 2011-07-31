--Filename: Util.hs
--Project: Rmmbr, a command-line program for handling reminders
--Author: Spencer Gordon
--Date: July 26th, 2011

module Util where

import Control.Monad
import Data.Array
import Data.Either( partitionEithers )
import Data.List( isSuffixOf )
import Data.Maybe( fromJust )
import Data.Time
import System.Console.ANSI
import System.Console.GetOpt
import System.Directory
import System.IO
import System.Locale
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Entry

appName = "rmmbr"
decoLength = 30
decoChar = '*'
defaultSort = "def"
defaultListFile = "todo.txt"
headerLength = 2
usageHelpHeader = "Usage: rmmbr [OPTION...] "


{-- Defining OptionTransformer and the handler for invalid arguments --}

type OptionTransformer = Options -> IO Options

throwUserError :: String -> OptionTransformer
throwUserError str = (\opts -> ioError $ userError $ "Couldn't parse the following argument: " ++ str)

doNonOptsHandling :: String -> OptionTransformer
doNonOptsHandling str opts = do (nonOptsHandler opts) str opts

{-- DataTypes --}

data Options = Options { nonOptsHandler :: String -> OptionTransformer,
                         showVerbose :: Bool,
                         helpCmd :: Maybe String,
                         overwrite :: Bool,
--                         interactive :: Bool,
                         position :: Either Position [Position],
                         theShowList :: Either String [String],
                         theList :: String,
                         entries :: [Entry],
                         importance :: String,
                         confirm :: String,
                         sort :: String }

data Position = First | Last | Index Int | RIndex Int | Range Int Int | RRange Int Int

{-- Takes the arguments and the list of options and returns the processed options structure and the unrecognized arguments --}

processInput :: [String] -> [OptDescr OptionTransformer] -> OptionTransformer -> IO (Options, [String])
processInput args options optSetup = case getOpt (ReturnInOrder doNonOptsHandling) options args of
                                          (optionTransformations, nonOpts, []) -> do opts <- foldl (>>=) (optSetup defaultOptions) optionTransformations
                                                                                     return (opts, nonOpts)
                                          (_,_,errors) -> ioError $ userError $ ((unlines errors) ++ (usageInfo usageHelpHeader options))

{-- The default Options type to be folded upon --}

defaultOptions :: Options
defaultOptions = Options { nonOptsHandler = throwUserError,
                           showVerbose = False,
                           helpCmd = Nothing,
                           overwrite = False,
--                           interactive = False,
                           position = Left Last,
                           theShowList = Left defaultListFile,
                           theList = defaultListFile,
                           entries = [],
                           importance = "",
                           sort = "" }

{-- Helper Functions --}

{-- Prompts until a 'y' or 'n' is received as input --}

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

{-- Creates the app directory and the default list --}

configure :: IO ()
configure = do putStrLn "Creating appropriate directories and deault list files..."
               (appDir, _) <- getDirAndLists
               createDirectoryIfMissing True appDir
               setCurrentDirectory appDir
               createList defaultListFile

getDirAndLists :: IO (FilePath, [String])
getDirAndLists = do appDir <- getAppUserDataDirectory appName
                    setCurrentDirectory appDir
                    dirContents <- getDirectoryContents appDir
                    files <- filterM doesFileExist dirContents
                    let lists = filter (".txt" `isSuffixOf`) files
                    return (appDir, lists)

{-- Takes a list and returns a tuple of the header and entries, in lines --}

getHeaderAndEntries :: FilePath -> IO ([String],[Entry])
getHeaderAndEntries file = do (appDir, _) <- getDirAndLists
                              setCurrentDirectory appDir
                              handle <- openFile file ReadMode
                              header <- replicateM headerLength $ hGetLine handle
                              entriesParse <- getEntries' file handle                              
                              hClose handle
                              entries <- case partitionEithers entriesParse of
                                   (_, vals) -> return vals
                                   (errs, _) -> inputError $ unlines $ map show errs
                              return (header, entries)

getEntries' :: FilePath -> Handle -> IO [Either ParseError Entry]
getEntries' file handle = do eof <- hIsEOF handle
                             if eof then return [] else do entryLine <- hGetLine handle
                                                           let e = parse entry file entryLine
                                                           entries <- getEntries' file handle
                                                           return (e:entries)

{-- Initializes a list with a header --}

createList :: FilePath -> IO ()
createList newList = do handle <- openFile newList WriteMode
                        currentTime <- getCurrentTime
                        hPutStrLn handle $ (replicate decoLength decoChar) ++ " " ++ newList ++ " " ++ (replicate decoLength decoChar)
                        hPutStrLn handle $ formatTime defaultTimeLocale "Created on %D at: %X" currentTime
                        hClose handle

{-- This is the full parser for a position argument, which just adds first and last to the other parsers --}

positionArg :: Parser Position
positionArg = do{ char 'f'
                ; optional $ string "irst"
                ; return First
                }
              <|>
              do{ char 'l'
                ; optional $ string "ast"
                ; return Last
                }
              <|> paren
              <|>
              do{ values <- numOrRange
                ; case values of
                       Left val -> return (Index val)
                       Right (fst, snd) -> return (Range fst snd) }

{-- Parses parens indicated positions from end, and constructs positions accordingly --}

paren :: Parser Position
paren = do{ char '('
          ; values <- numOrRange
          ; char ')'
          ; case values of
                 Left val -> return (RIndex val)
                 Right (fst, snd) -> return (RRange fst snd) }

{-- parses a number or span, and returns a single number or a duple in an Either --}

numOrRange :: Parser (Either Int (Int, Int))
numOrRange = do{ start <- many1 digit
               ; do{ char '-'
                   ; end <- many1 digit
                   ; return (Right ((read start), (read end)))
                   }
               <|> return (Left (read start)) }


{-- This takes a position, decomposes it, and marks it or throws errors appropriately --}

removePositionFromList :: Array Int (Bool, a) -> Position -> Array Int (Bool, a)
removePositionFromList arr pos = let end = (snd (bounds arr)) in
                                 case pos of
                                      First -> markEntryOrThrowError 1 arr
                                      Last -> markEntryOrThrowError end arr
                                      Index i -> markEntryOrThrowError i arr
                                      RIndex i -> markEntryOrThrowError (end - i) arr
                                      Range fst snd -> if fst > snd then error "The beginning of an interval must be smaller than the end"
                                                                    else foldl (\a i -> markEntryOrThrowError i a) arr [fst..snd]
                                      RRange fst snd -> if fst > snd then error "The beginning of an interval must be smaller than the end"
                                                                     else foldl (\a i -> markEntryOrThrowError i a) arr [(end - snd)..(end - fst)]


{-- It marks a position in the array as slated for removal or throws an error --}

markEntryOrThrowError :: Int -> Array Int (Bool, a) -> Array Int (Bool, a)
markEntryOrThrowError i arr = if i `notElem` (indices arr) then error "Invalid position entered"
                                                           else case (arr ! i) of
                                                                     (True, _) -> error "The same position marked twice. Aborting"
                                                                     (False, entry) -> arr // [(i, (True, entry))]

{-- a wrapper around ioError and userError --}

inputError :: String -> IO a
inputError msg = ioError $ userError msg

