module AOC.Y2021.Day7 where
import Library

main = main7 Full

main7 source = do
  crabPositions :: [Int] <- read . (<> "]") . ("[" <>) <$> readFile21
    ("d7.input" <> show source)
  printPart 1 $ minimum
    [ sum $ abs . subtract c <$> crabPositions
    | c <- [1 .. maximum crabPositions]
    ]
  printPart 2 $ minimum
    [ sum $ (\n -> n * (n + 1) `div` 2) . abs . subtract c <$> crabPositions
    | c <- [1 .. maximum crabPositions]
    ]

 where
   -- got to love pointfree.io
  printPart = (print .) . (. show) . (<>) . (<> " :") . ("part " <>) . show
