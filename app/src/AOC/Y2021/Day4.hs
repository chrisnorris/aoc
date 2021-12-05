module AOC.Y2021.Day4 where

import Library
import qualified Data.Array as A

type PairInts = (Int, Int)

main = main4 >>= mapM print

main4 :: IO (A.Array PairInts [PairInts])
main4 = do
   (nums : boards) <- readInstr
   let game :: [Int] = read <$> wordsWhen (== ',') nums
   let allBoards = A.listArray ((0,0), (2,4)) (process $ filter (/= "") boards)
   
   return $ play game allBoards
   
 where   play [] board = board
         play (round : games) boards = 
            let thisRound = playANumber boards round in
            if theresAWinner thisRound then thisRound
            else play games thisRound
   
         playANumber board n = (\e -> (\(a, b) -> if a == n then (a, 1) else (a,b)) <$> e) <$> board

theresAWinner round = True `elem` [
   let board = getNthBoard round n
       boardAsArray = A.listArray ((0,0), (4,4)) (concat board)
       zeros = repeat 0
       diag = [(A.!) boardAsArray (i, i) | i <- [0..4]]
       ver =  [[(A.!) boardAsArray (i,j) | j <- [0..4] ] | i <- [0..4]]
       hor =  [[(A.!) boardAsArray (j,i) | j <- [0..4] ] | i <- [0..4]] in

   (elem 5 $ sum . map snd <$> (ver <> hor <> [diag])) | n <- [0..2]]

getNthBoard boards n = [(A.!) boards (n,x) | x <- [0..4]]


readInstr = inp21Str "d4S.input"

process boardInput = map ( (,0) . read) . words <$> boardInput