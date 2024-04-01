module AOC.Y2021.Day13 where

import qualified Data.Array as A
import Library

-- NOT TO BE COMMITTED
main = main13 Full

main13 source = do
  ins <- map (drop 2 . words) . filter ('=' `elem`) <$> getInput source

  s2 :: [(Int, Int)] <-
    map (\x -> read $ "(" <> x <> ")") . takeWhile (/= "") <$> getInput source

  let (xd, yd) = (maximum $ fst <$> s2, maximum $ snd <$> s2)

  let dots = (,1) <$> s2
  let flipy' row input =
        let ps = [a | a@((x, y), e) <- A.assocs input, y > row]
         in ( ( \(p@(x, y), e) ->
                  let flipped = (x, row - (y - row))
                      fVal = input A.! flipped
                   in (flipped, merge fVal e)
              )
                <$> ps
            )
              <> (ps <&> fmap (const 0))

  let flipx' col input =
        let ps = [a | a@((x, y), e) <- A.assocs input, x > col]
         in ( ( \(p@(x, y), e) ->
                  let flipped = (col - (x - col), y)
                      fVal = input A.! flipped
                   in (flipped, merge fVal e)
              )
                <$> ps
            )
              <> (ps <&> fmap (const 0))

  let flipy row input =
        let ps = [a | a@((x, y), e) <- A.assocs input, y > row]

            x =
              ( \(p@(x, y), e) ->
                  let flipped = (x, row - (y - row))
                      fVal = input A.! flipped
                   in (flipped, merge fVal e)
              )
                <$> ps
            xmax = maximum $ fst . fst <$> x
            xmin = minimum $ fst . fst <$> x
            ymax = maximum $ snd . fst <$> x
            ymin = minimum $ snd . fst <$> x
         in A.array ((0, 0), (xmax - xmin, ymax - ymin)) $
              (\((x, y), e) -> ((x - xmin, y - ymin), e))
                <$> x

  let flipx col input =
        let ps = [a | a@((x, y), e) <- A.assocs input, x > col]

            x =
              ( ( \(p@(x, y), e) ->
                    let flipped = (col - (x - col), y)
                        fVal = input A.! flipped
                     in (flipped, merge fVal e)
                )
                  <$> ps
              )
            xmax = maximum $ fst . fst <$> x
            xmin = minimum $ fst . fst <$> x
            ymax = maximum $ snd . fst <$> x
            ymin = minimum $ snd . fst <$> x
         in A.array ((0, 0), (xmax - xmin, ymax - ymin)) $
              (\((x, y), e) -> ((x - xmin, y - ymin), e))
                <$> x

  let origami =
        ( \case
            ["x", d] -> (flipx (read d :: Int))
            ["y", d] -> (flipy (read d :: Int))
        )
          . concatMap (wordsWhen (== '='))
          <$> ins

  let zeroedArray = A.listArray ((0, 0), (xd, yd)) (repeat 0)
  let final = (A.//) zeroedArray dots

  let applyFolds = foldl (\arr afold -> afold arr) final origami

  -- print $ applyFolds

  let ((x1, y1), (x2, y2)) = A.bounds applyFolds

  forM (pretty applyFolds (x2 - x1) (y2 - y1)) putStrLn
  where
    getInput source = inp21Str ("d13.input" <> show source)
    merge :: Int -> Int -> Int
    merge p q = fromEnum $ toEnum p || toEnum q
    transpose arr xd yd =
      A.array ((0, 0), (yd, xd)) [((y, x), e) | ((x, y), e) <- A.assocs arr]
    pretty arr xd yd =
      chunksOf
        (xd + 1)
        [ if e == 1 then '█' else '-'
          | ((x, y), e) <- A.assocs (transpose arr xd yd)
        ]
