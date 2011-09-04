--Filename: Util.hs
--Project: Rmmbr, a command-line program for handling reminders
--Author: Spencer Gordon
--Date: July 26th, 2011

module Base.Util where

import Control.Monad
import Data.Array
import Data.Either( partitionEithers )
import Data.List( isSuffixOf, sortBy )
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

import Base.Entry

appName = "rmmbr"
decoLength = 30
decoChar = '*'
defaultSort = "byDateTime"
defaultListName = "todo"
headerLength = 2
usageHelpHeader = "Usage: rmmbr [OPTION...] "
defaultComment = "(default comment)"
defaultExtension = "rmmbr"

type ListName = String

{-- Defining OptionTransformer and the handler for invalid arguments --}

type OptionTransformer = Options -> IO Options

throwUserError :: String -> OptionTransformer
throwUserError str = (\opts -> ioError $ userError $ "Couldn't parse the following argument: " ++ str)

doNonOptsHandling :: String -> OptionTransformer
doNonOptsHandling str opts = do (nonOptsHandler opts) str opts

{-- DataTypes --}

data Options = Options { nonOptsHandler :: String -> OptionTransformer,
                         verbose :: Bool,
                         helpCmd :: Maybe String,
                         comment :: String,
                         overwrite :: Bool,
--                         interactive :: Bool,
                         position :: Either Position [Position],
                         theShowList :: Either ListName [ListName],
                         theList :: ListName,
                         entries :: [Entry],
                         importance :: String,
                         confirm :: String,
                         sort :: SortFunction }

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
                           verbose = False,
                           helpCmd = Nothing,
                           comment = defaultComment,
                           overwrite = False,
--                           interactive = False,
                           position = Left Last,
                           theShowList = Left defaultListName,
                           theList = defaultListName,
                           entries = [],
                           importance = "",
                           sort = fromJust $ lookup defaultSort sortOrders }

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
               createList defaultListName

getDirAndLists :: IO (FilePath, [ListName])
getDirAndLists = do appDir <- getAppUserDataDirectory appName
                    setCurrentDirectory appDir
                    dirContents <- getDirectoryContents appDir
                    files <- filterM doesFileExist dirContents
                    let (lists, _) = unzip $ map (break (\char -> char == '.')) $ filter (defaultExtension `isSuffixOf`) files
                    return (appDir, lists)

{-- Takes a list and returns a tuple of the header and entries, in lines --}

getHeaderAndEntries :: ListName -> SortFunction -> IO ([String],[Entry])
getHeaderAndEntries name sf = do (appDir, _) <- getDirAndLists
                                 setCurrentDirectory appDir
                                 handle <- openList name ReadMode
                                 header <- replicateM headerLength $ hGetLine handle
                                 entriesParse <- getEntries' name handle                              
                                 hClose handle
                                 entries <- case partitionEithers entriesParse of
                                                ([], vals) -> return vals
                                                (errs, _) -> inputError $ unlines $ map show errs                                 
                                 return (header, sortBy sf entries)

getEntries' :: ListName -> Handle -> IO [Either ParseError Entry]
getEntries' name handle = do eof <- hIsEOF handle
                             if eof then return [] else do entryLine <- hGetLine handle
                                                           let e = parse entry name entryLine
                                                           entries <- getEntries' name handle
                                                           return (e:entries)

{-- Initializes a list with a header --}

createList :: ListName -> IO ()
createList name = do handle <- openList name WriteMode
                     currentTime <- getCurrentTime
                     hPutStrLn handle $ (replicate decoLength decoChar) ++ " " ++ name ++ " " ++ (replicate decoLength decoChar)
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
removePositionFromList arr pos = applyToPosition pos markEntryOrThrowError arr

{-- It marks a position in the array as slated for removal or throws an error --}

markEntryOrThrowError :: Int -> Array Int (Bool, a) -> Array Int (Bool, a)
markEntryOrThrowError i arr = if i `notElem` (indices arr) then error "Invalid position entered"
                                                           else case (arr ! i) of
                                                                     (True, _) -> error "The same position marked twice. Aborting"
                                                                     (False, entry) -> arr // [(i, (True, entry))]

{-- a wrapper around ioError and userError --}

inputError :: String -> IO a
inputError msg = ioError $ userError msg

{-- Represents an interval that is backwards --}  

badIntervalError = error "The beginning of an interval must be smaller than the end"

{-- an abstraction that allows an action to be applied to a list of entries at a position --}

applyToPosition :: Position -> (Int -> Array Int e -> Array Int e) -> Array Int e -> Array Int e
applyToPosition First f arr = f 1 arr
applyToPosition Last f arr = f (snd $ bounds arr) arr
applyToPosition (Index i) f arr = f i arr
applyToPosition (RIndex i) f arr = f ((snd $ bounds arr) - i) arr
applyToPosition (Range s e) f arr = if s > e then badIntervalError
                                             else foldl (flip f) arr [s..e]
applyToPosition (RRange s e) f arr = if s > e then badIntervalError
                                              else foldl (flip f) arr [((snd $ bounds arr) - s)..((snd $ bounds arr) - e)]

{-- Does nothing if list is valid. error otherwise --}

validateList :: ListName -> [ListName] -> IO ()
validateList list lists = if list `elem` lists then return ()
                                               else inputError $ "The selected list does not exist: " ++ list


appendExtension :: String -> String
appendExtension = (++ ('.':defaultExtension))

openList :: String -> (IOMode -> IO Handle)
openList name = openFile (appendExtension name)


