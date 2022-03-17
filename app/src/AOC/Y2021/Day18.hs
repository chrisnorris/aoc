module AOC.Y2021.Day18 where

import           Library                        ( inp21Str
                                                , forM
                                                , liftM2
                                                , Source(..)
                                                )
import           Data.Char                      ( digitToInt )
import qualified Data.Array                    as A

main = main18 Sample >>= print -- >> main10ii Full >>= print

main18 source = do

--  let r = [1,2] ++ [[3,4],5]
  (xd, yd)   <- liftM2 (,) length (length . head) <$> getInput source

  inputArray <-
    A.listArray ((0, 0), (xd - 1, yd - 1))
      <$> (map digitToInt . concat <$> getInput source)
  return $ inputArray
  
getInput source = inp21Str ("d18.input" <> show source)
