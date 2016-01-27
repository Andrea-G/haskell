module AssignmentsAndSubmissions
  (createAssignment
  , listSubmissions
  , getSubmission
  , getConfiguration
  , listFiles
  , getSubmissionPath
  , upload
  , Assignment(..)
  , Configuration(..)
  , Submission(..)
  , Type(..)
  
) where

import Data.Char
import Data.List
import Control.Monad
import Data.Text (Text, unpack, pack)
import System.IO
import System.Directory
import System.FilePath
import Data.Maybe
import Data.Time

-- | Academic year shorthand (e.g. 2015 for 2015/16)
type Year = Integer

-- | An assignment type
data Type = Homework | Exam | Project deriving (Show, Eq)
-- | A an assignment configuration data structure
-- | Uses Data.Time.UTCTime
-- | If files is an empty list, ANY number of files are OK

data Configuration = Configuration { published :: UTCTime
                      , deadline :: UTCTime
                      , lateDeadline :: UTCTime
                      , files :: [String]
                      , minScore :: Double
                      , maxScore :: Double
                      , required :: Double
                      } deriving (Show)

-- | An assignment descriptor
data Assignment = Assignment { year :: Year
                  , assignmentType :: Type
                  , number :: Int
                  } deriving (Show, Eq)

-- | A user identifier (not DB id) like a username or JMBAG
type UserIdentifier = String

data Submission = Submission{ assignment :: Assignment
                            , userID :: UserIdentifier
                            , subFiles :: [String]
                            } deriving (Show)

-- | Checks if the file is .pdf file
checkPdf f = if ext == "pdf" then f else error "PDF file expected!"
   where ext = drop (length f - 3) f 

-- | Creates configuration file from Configuration object
createConfigFile :: Configuration -> FilePath -> IO ()
createConfigFile config fpath = writeFile (fpath ++ "/.config") $ unlines $ contents
   where contents = dates ++ scores ++ [unlines (files config)]
         dates = map show  $ sequence [published, deadline, lateDeadline] config
         scores = map show $ sequence [minScore, maxScore, required] config

-- | Computes a file path for an assignment
getAssignmentPath a = show(year a) ++ "/" ++ show(assignmentType a) ++ "/" ++ show(number a)

-- | Creates a new assignment from Assignment, configuration and PDF file
-- | The PDF file should be copied, moved or symlinked so that it is
-- | accessible from the assignment directory.
createAssignment :: Assignment -> Configuration -> FilePath -> IO ()
createAssignment a config file = do
   createDirectoryIfMissing True assignmentPath
   createConfigFile config assignmentPath
   copyFile file pdfPath
   where assignmentPath = getAssignmentPath a
         pdfPath = assignmentPath ++ "/" ++ (checkPdf file)

-- | Lists the user identifiers for submissions made for an assignment
listSubmissions :: Assignment -> IO [UserIdentifier]
listSubmissions a = do
  files <- getDirectoryContents dirPath
  let realFiles = filter (`notElem` [".", ".."]) files
  dirs <- forM realFiles $ \f -> do
     isDir <- doesDirectoryExist (dirPath ++ "/" ++ f)
     if isDir
     then return f else return ""
  return (filter (/="") dirs)
    where dirPath = getAssignmentPath a

-- | Views a single submission in detail
getSubmission :: Assignment -> UserIdentifier -> IO Submission
getSubmission a id = do
  files <- getDirectoryContents dirPath
  let realFiles = filter (`notElem` [".", ".."]) files
  return (Submission a id realFiles)
  where dirPath = (getAssignmentPath a) ++ "/" ++ id

-- | Gets the configuration object for an assignment
getConfiguration :: Assignment -> IO Configuration
getConfiguration a = withFile f ReadMode $ \h -> do
  published <- hGetLine h
  deadline <- hGetLine h
  lateDeadline <- hGetLine h
  minScore <- hGetLine h
  maxScore <- hGetLine h
  required <- hGetLine h
  files <- hGetContents h
  print files
  let realFiles = filter (/="") (lines files)
  return (Configuration ((read published)::UTCTime) ((read deadline)::UTCTime) ((read lateDeadline)::UTCTime) realFiles ((read minScore)::Double) ((read maxScore)::Double) ((read required)::Double))
  where f = (getAssignmentPath a) ++ "/.config" 

-- | Lists the files contained in a submission
listFiles :: Submission -> IO [FilePath]
listFiles s = return (subFiles s)

-- | Computes a file path for a submission
getSubmissionPath :: Submission -> FilePath
getSubmissionPath s = getAssignmentPath a ++ "/" ++ (userID s)
    where a = assignment s

-- | Given a solution file body, adds a solution directory/file to the
-- | directory structure of an assignment. It will indicate an error
-- | (using Maybe, exceptions, or some other mechanism) if the file is
-- | not in a defined permitted list. It will override already made
-- | submissions.
-- | Assignment -> File Body -> File Name -> Error indication (?)
upload :: Assignment -> UserIdentifier -> Text -> String -> IO (Maybe Submission)
upload a id text fname = do
 config <- getConfiguration a
 let filesList = files config
 if not (null filesList) && (fname `notElem` filesList)
     then return Nothing
     else do 
        sub <- createSubmission a id text fname
        return (Just sub)

-- | Adds a solution and directory (if missing) to the assignment directory and returns that submission
createSubmission :: Assignment -> UserIdentifier -> Text -> String -> IO Submission
createSubmission a id text fname = do
     createDirectoryIfMissing False subPath
     writeFile (subPath ++ "/" ++ fname) $ unpack text
     sub <- getSubmission a id
     return sub
         where subPath = ((getAssignmentPath a) ++ "/" ++ id)

a1 = Assignment 2016 Homework 3
config1 = Configuration ((read "2011-11-19 18:28:52.607875 UTC")::UTCTime) ((read "2011-11-19 18:28:53.607875 UTC")::UTCTime) ((read "2011-11-19 18:28:54.607875 UTC")::UTCTime) ["Homework.hs"] 5 7 6
