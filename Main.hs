module Test where

import System.Directory
import Control.Monad

getFiles :: FilePath -> IO [FilePath]
getFiles dir = (map (\x -> dir ++ "/" ++ x) <$> removeHiddenFiles <$> listDirectory dir) >>= filterM doesFileExist


removeHiddenFiles :: [FilePath] -> [FilePath]
removeHiddenFiles = filter (\x -> head x /= '.')
