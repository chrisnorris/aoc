module AOC.Y2021.Day4 where

import Library
import qualified Data.Array as A

main = main4
-- sam 4512
-- full 41503
main4 = do
   (nums : rawBoards) <- readInstr
   let game :: [Int] = read <$> wordsWhen (== ',') nums
   let boards = process $ filter (/= "") rawBoards
   let noCards = round $ fromIntegral (length boards) / 5
   let allBoards = A.listArray ((0,0), (noCards - 1, 4)) boards
   
   let ([(_, _, sumUnmarkeds)], round) = play game allBoards in print $ round * sumUnmarkeds
   
 where   play [] board = ([(True, 0, 0)], 0)
         play (round : games) boards =
            let thisRound = playANumber boards round in
            if fst3 $ head $ theresAWinner thisRound then
                 (theresAWinner thisRound, round) else play games thisRound
   
         playANumber board n = (\e -> (\(a, b) -> if a == n then (a, 1) else (a,b)) <$> e) <$> board

theresAWinner round = 
   let winners = [
           let board = getNthBoard round n
               boardAsArray = A.listArray ((0,0), (4,4)) (concat board)
               zeros = repeat 0
               diag = [(A.!) boardAsArray (i, i) | i <- [0..4]]
               ver =  [[(A.!) boardAsArray (i,j) | j <- [0..4] ] | i <- [0..4]]
               hor =  [[(A.!) boardAsArray (j,i) | j <- [0..4] ] | i <- [0..4]] 
               unmarkeds = fst <$> filter ( (== 0) . snd) (concat board) in

           (elem 5 $ sum . map snd <$> (ver <> hor <> [diag]), n, sum unmarkeds) | n <- [0..99]] in
   if True `elem` (fst3 <$> winners) then filter ( (== True) . fst3) winners else [(False, 0, 0)]

getNthBoard boards n = [(A.!) boards (n,x) | x <- [0..4]]

-- use [0..2] above for sam, [0.99] full
readInstr = inp21Str "d4.input"

process boardInput = map ( (,0) . read) . words <$> boardInput
