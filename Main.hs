module Test where

import System.Directory
import System.Environment
import Control.Monad
import qualified Data.Map as M

-- TODO: Have a mode that checks mime types instead of extensions

fileExtensions :: M.Map String String
fileExtensions = M.fromList [(".txt", "Text"), (".hs", "Haskell")]

getFiles :: FilePath -> IO [FilePath]
getFiles dir = (removeHiddenFiles <$> listDirectory dir) >>= filterM (\x -> doesFileExist (dir ++ "/" ++ x))

removeHiddenFiles :: [FilePath] -> [FilePath]
removeHiddenFiles = filter (\x -> head x /= '.')

getFilename :: FilePath -> FilePath
getFilename = fst . break (=='.')

getFileExtension :: FilePath -> FilePath
getFileExtension = snd . break (=='.')

lookupFileExtension :: FilePath -> FilePath
lookupFileExtension file = case (M.lookup (getFileExtension file) fileExtensions) of
  Just x -> x
  Nothing -> "Other"

getNewPath :: FilePath -> [FilePath] -> [FilePath]
getNewPath cwd = map (\file -> cwd ++ "/" ++ (lookupFileExtension file) ++ "/" ++ file)

getOldPath :: FilePath -> [FilePath] -> [FilePath]
getOldPath cwd = map (\file -> cwd ++ "/" ++ file)

createDirectories :: FilePath -> [FilePath] -> IO [()]
createDirectories dir files = mapM (createDirectoryIfMissing True) (map (\x -> dir ++ "/" ++ lookupFileExtension x) files)

moveFiles :: FilePath -> [FilePath] -> IO [()]
moveFiles dir files = mapM (\(old,new) -> renamePath old new) (zip (getOldPath dir files) (getNewPath dir files))

getFirstArg :: IO String
getFirstArg = fmap (!!1) getArgs

main :: FilePath -> IO ()
main dir = do
  -- Get Files
   <- getFirstArg
  files <- getFiles dir
  -- Create Directories From Files
  dirs <- createDirectories dir files
  -- Move Files to Directories
  moveFiles dir files
  print "Done"
