module Lib
  ( readFile',
  )
where

import System.FilePath.Posix ((</>))

readFile' :: FilePath -> IO String
readFile' file = readFile ("app/2020/input" </> file)
