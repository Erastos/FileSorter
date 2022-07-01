module Test where

import System.Directory
import Control.Monad
import qualified Data.Map as M

-- TODO: Have a mode that checks mime types instead of extensions

fileExtensions :: M.Map String String
fileExtensions = M.fromList [(".txt", "Text"), (".hs", "Haskell")]

getFiles :: FilePath -> IO [FilePath]
getFiles dir = (removeHiddenFiles <$> listDirectory dir) >>= filterM (\x -> doesFileExist (dir ++ "/" ++ x))


removeHiddenFiles :: [FilePath] -> [FilePath]
removeHiddenFiles = filter (\x -> head x /= '.')

getNewPath :: FilePath -> [FilePath] -> [FilePath]
getNewPath cwd files = map (\file -> determineLoc file (M.lookup (snd $ break (=='.') file) fileExtensions)) files
  where
    determineLoc file (Just x) = cwd ++ "/" ++ x ++ "/" ++ file
    determineLoc file Nothing =  cwd ++ "/Other/" ++ file
