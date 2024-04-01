module AOC.Y2021.Day17 where

import Control.Arrow (second)
import Library (Source (..))

main = main17 Sample >>= print >> main17 Full >>= print

main17 :: Source -> IO (Int, Int)
main17 source = do
  let (xmi, xma, ymi, yma) = case source of
        Sample -> (20, 30, -10, -5)
        Full -> (29, 73, -248, -194)

  let velocities = [(i, j) | i <- [5 .. xma], j <- [ymi .. 1000]]

      inZone (x, y) = (x >= xmi && x <= xma) && (y >= ymi && y <= yma)
      allPaths = (\x -> (x, project (xma, ymi) (0, 0) [] x)) <$> velocities
      pathsInZone =
        second (maximum . (snd <$>)) <$> filter (inZone . head . snd) allPaths

  return $ (length pathsInZone, maximum $ snd <$> pathsInZone)

project m@(xma, ymi) (x, y) path vel | x > xma || y < ymi = drop 1 path
project m@(xma, ymi) (x, y) path (xVel, yVel) =
  let p@(newX, newY) = (x + xVel, y + yVel)
   in project m p (p : path) (if xVel > 0 then xVel - 1 else xVel, yVel - 1)
