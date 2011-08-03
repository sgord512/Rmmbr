module Base.Entry where

import Control.Monad
import Data.Maybe( fromJust )
import Data.Time
import Data.Tuple( swap )
import System.Console.ANSI
import System.Locale
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Base.Toolbox( withColor, withBgColor )

dateTimeString = "%m/%d/%Y (%R %P)"
lBracket = '['
rBracket = ']'

{-- The definition of Entry data --}

data Entry = Entry AddTime Title CompletionStatus Importance Comment

{-- Title and Comment are given newtypes so that I can override their default show methods --}

newtype Title = MakeTitle String
        deriving (Eq, Ord)
newtype Comment = MakeComment String
        deriving (Eq, Ord)

{-- AddTime should be more complex to take into account timezones, or localization of some sort --}

data AddTime = MakeAddTime UTCTime
     deriving (Eq, Ord)

data Importance = Low | Medium | High
     deriving (Eq, Ord, Bounded, Enum)

data CompletionStatus = NotStarted | InProgress | Complete
     deriving (Eq, Ord, Bounded, Enum)

{-- Parser for entries --}

entry :: Parser Entry
entry = do{ utcString <- (liftM unwords) ((many1 $ try $ noneOf [' ','-']) `endBy1` (char ' ')) <?> "time and date" 
          ; char '-' >> space
          ; importance <- importanceParser
          ; spaces
          ; completionStatus <- between (char lBracket) (char rBracket) compStatusParser
          ; space >> char '-' >> space
          ; title <- manyTill anyChar (char ':') <?> "title"
          ; space
          ; comment <- manyTill anyChar eof <?> "comment"
          ; return (Entry (MakeAddTime (readTime defaultTimeLocale dateTimeString utcString))
                          (MakeTitle title)
                          completionStatus
                          importance
                          (MakeComment comment))
          }

compStatusParser :: Parser CompletionStatus
compStatusParser = liftM (rLookupSafe compStatusCharMap) (oneOf $ map (\(_, c) -> c) compStatusCharMap) <?> "completion status"

compStatusCharMap :: [(CompletionStatus, Char)]
compStatusCharMap = [(Complete, 'X')
                    ,(InProgress, '~')
                    ,(NotStarted, ' ')
                    ]

importanceParser :: Parser Importance
importanceParser = liftM (rLookupSafe importanceCharMap) (oneOf $ map (\(_, c) -> c) importanceCharMap) <?> "importance"

importanceCharMap :: [(Importance, Char)]
importanceCharMap = [(High, 'H')
                    ,(Medium, 'M')
                    ,(Low, 'L')
                    ]


{-- A helper function that reverses an association list to look up keys by entry, and then pulls the result out of a Maybe --}

rLookupSafe :: [(a, Char)] -> Char -> a
rLookupSafe assocList = fromJust . ((flip lookup) (map swap assocList))

{-- The show methods for all the data I am working with --}

instance Show Entry where
         show (Entry addTime title compStatus importance comment) = show addTime ++
                                                                    " - " ++
                                                                    show importance ++
                                                                    " " ++
                                                                    show compStatus ++
                                                                    " - " ++
                                                                    show title ++
                                                                    ": " ++
                                                                    show comment

instance Show Importance where
         show i = [fromJust $ lookup i importanceCharMap]

instance Show CompletionStatus where
         show c = lBracket:(fromJust $ lookup c compStatusCharMap):rBracket:[]

instance Show AddTime where
         show (MakeAddTime addTime) = formatTime defaultTimeLocale dateTimeString addTime

instance Show Title where
         show (MakeTitle t) = t

instance Show Comment where
         show (MakeComment c) = c

{-- I can use instances of this class to have Entries print themselves as necessary --}

class CP a where
      colorPrint :: a -> IO ()

instance CP Entry where
         colorPrint (Entry addTime title compStatus importance comment) = sequence_  [colorPrint addTime
                                                                                     ,putStr " - "
                                                                                     ,colorPrint importance
                                                                                     ,putStr " "
                                                                                     ,colorPrint compStatus
                                                                                     ,putStr " - "
                                                                                     ,colorPrint title
                                                                                     ,putStr ": "
                                                                                     ,colorPrint comment
                                                                                     ,putStrLn ""
                                                                                     ]

instance CP AddTime where
         colorPrint = putStr . show

instance CP Importance where
         colorPrint i = do let color = case i of
                                            High -> Blue
                                            Medium -> Cyan
                                            Low -> White
                           (putChar `withColor` color) (fromJust $ lookup i importanceCharMap)

instance CP CompletionStatus where
         colorPrint c = do let color = case c of
                                            NotStarted -> Red
                                            InProgress -> Cyan
                                            Complete -> Green
                           putChar lBracket
                           (putChar `withColor` color) (fromJust $ lookup c compStatusCharMap)
                           putChar rBracket
instance CP Title where
         colorPrint (MakeTitle t) = putStr t

instance CP Comment where
         colorPrint (MakeComment c) = putStr c

{-- Creates a new entry with a different comment --}

overwriteComment :: String -> Entry -> Entry 
overwriteComment str (Entry addTime title compStatus importance _) = (Entry addTime title compStatus importance (MakeComment str)) 

{-- Creates a new entry with a comment appended to the comment of the provided entry --} 

appendComment :: String -> Entry -> Entry
appendComment str (Entry addTime title compStatus importance (MakeComment com)) = (Entry addTime title compStatus importance (MakeComment (com ++ "; " ++  str)))

{-- Creates a new entry with a new completion status --}

changeCompStatus :: CompletionStatus -> Entry -> Entry
changeCompStatus compStatus (Entry addTime title _ importance comment) = (Entry addTime title compStatus importance comment)

{-- Sorting orders and parsing --}

type SortFunction = Entry -> Entry -> Ordering

sortOrders :: [(String, SortFunction)]
sortOrders = [("byCompletion", (\(Entry _ _ compA _ _) (Entry _ _ compB _ _) -> compA `compare` compB))
             ,("byImportance", (\(Entry _ _ _ impA _) (Entry _ _ _ impB _) -> impA `compare` impB))
             ,("byTitle", (\(Entry _ titleA _ _ _) (Entry _ titleB _ _ _) -> titleA `compare` titleB))
             ,("byDateTime", (\(Entry timeA _ _ _ _) (Entry timeB _ _ _ _) -> timeA `compare` timeB))
             ]


sortArg :: Parser SortFunction
sortArg = parenRev
          <|> 
          sortMethodParse
          <?> "sort method"


parenRev :: Parser SortFunction
parenRev = do{ char '('
             ; values <- sortMethodParse
             ; char ')'
             ; return (flip values)
             }

sortMethodParse :: Parser SortFunction
sortMethodParse = (liftM (fromJust . (flip lookup) sortOrders)) (foldl1 (<|>) (map (\(s, _) -> (try (string s))) sortOrders))
                   <?> "sortMethod"


