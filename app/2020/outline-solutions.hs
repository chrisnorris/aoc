#!/usr/bin/env stack
{- stack --resolver lts-13.19 --install-ghc
      exec ghci
      --package regex-tdfa
      --package lens
      --package parsec
      --package data-clist
      --package filepath
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Lens as L
import Control.Monad (ap, foldM, forM, forM_, liftM2)
import qualified Data.Array as A
import Data.Bits
import Data.Bits.Lens as L
import qualified Data.CircularList as CL
import Data.Either (fromRight)
import Data.List (drop, intersect, intersperse, nub, sort, sortBy, take, unfoldr)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid (Last (..))
import qualified Data.Set as Set
import System.FilePath.Posix
import System.IO
import qualified Text.Parsec as P
import Text.Regex.TDFA

readFile' file = readFile ("input" </> file)

main =
  (\num -> nub $ traverse %~ sumOf each $ [(x, y, z) | x <- num, y <- num, z <- num, x + y + z == 2020])
    <$> (map read . lines <$> readFile' "d1.input")

parseLine s =
  let (a, b) = break (== ' ') s
      (c, d) = break (== ':') b
      [z] = drop 1 c
      (e, f) = break (== '-') a
      ste = filter (== z) (drop 2 d)
      str = length ste
      i1 = read e :: Int
      i2 = read (drop 1 f) :: Int
   in (i1, i2, str, (str >= i1) && (str <= i2))

parseLine2 s =
  let (a, b) = break (== ' ') s
      (c, d) = break (== ':') b
      [z] = drop 1 c
      (e, f) = break (== '-') a
      ste = filter (== z) (drop 2 d)
      str = length ste
      i1 = read e :: Int
      stf = drop 2 d
      xor' a b = (a || b) && not (a && b)
      i2 = read (drop 1 f) :: Int
   in (i1, i2, stf, z, xor' (stf !! (i1 -1) == z) (stf !! (i2 -1) == z))

main2 =
  map parseLine . lines <$> readFile' "d2.input"

main3 = do
  i <- lines <$> readFile' "d3.input"
  let steps (r, d) =
        (0, 0)
          ^.. unfolded
            ( \(x, y) ->
                if y < length i then _Just # ((x, y), (x + r, y + d)) else Nothing
            )

      slope (x, y) st =
        danger (x `divMod` length (head i))
        where
          danger (\(b, p) -> repeat i ^? ix b . ix y . ix p -> Just n) = n : st

  return
    . product
    $ [ length [x | x <- foldr slope [] (steps slopeC), x == '#']
        | slopeC <- ((,1) <$> [1, 3, 5, 7]) <> [(1, 2)]
      ]

main4 =
  readFile' "d4.input"
    >>= (return . length . filter (== True))
      . map (getIds . words)
      . lines
      . parseFile
      . lines

main4b = do
  valids <-
    map snd . filter ((True ==) . fst) . map (getId4b . words) . lines . parseFile . lines
      <$> readFile' "d4.input"
  return (valids, map validateMap valids)

main6a =
  map (length . Set.fromList . concat . words)
    . lines
    . parseFile
    . lines
    <$> readFile' "d6a.input"

main6b =
  readFile' "d6a.input"
    >>= (return . sum)
      . map (length . getIntersects . words)
      . lines
      . parseFile
      . lines

main8 =
  readFile' "d8.input" >>= interpret 0 0 . map ((,1) . parseOperand) . lines

interpret pc acc instructions =
  if pc == length instructions
    then return (acc, "TERMS")
    else case (instructions !! pc, pc) of
      (x@(Jmp n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + n)
            acc
            (take pc instructions ++ [(Jmp n, 2)] ++ drop (pc + 1) instructions)
      (x@(Jmp n, 2), _) -> return (acc, "LOOP")
      (x@(Acc n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + 1)
            (acc + n)
            (take pc instructions ++ [(Acc n, 2)] ++ drop (pc + 1) instructions)
      (x@(Acc n, 2), _) -> return (acc, "LOOP")
      (x@(Nop n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + 1)
            acc
            (take pc instructions ++ [(Nop n, 2)] ++ drop (pc + 1) instructions)
      ((Nop n, 2), _) -> return (acc, "LOOP")

main8b = do
  baseCodes <- map ((,1) . parseOperand) . lines <$> readFile' "d8.input"
  let allCodes = zip [0 ..] baseCodes
  forM
    ( foldr
        ( \(i, ins) acc -> case ins of
            (Jmp n, 1) ->
              (take i baseCodes ++ [(Nop n, 1)] ++ drop (i + 1) baseCodes) : acc
            (Nop n, 1) ->
              (take i baseCodes ++ [(Jmp n, 1)] ++ drop (i + 1) baseCodes) : acc
            _ -> acc
        )
        []
        allCodes
    )
    $ interpretb 0 0

main9 = do
  baseCodes :: [Int] <- map read . lines <$> readFile' "d9.input"
  let preamble = take 25 baseCodes
  let input = drop 25 baseCodes
  getValids preamble input

main10 = undefined

main11 = do
  x <- concat . lines <$> readFile' "d11.input"
  y <- concat . lines <$> readFile' "d11.input.sam2"

  let arrInit = A.listArray ((0, 0), (97, 94)) x

      valid (x, y) = x >= 0 && y >= 0 && x < 98 && y < 95
      adjacents (x, y) arr =
        [ arr A.! (x + m, y + n)
          | m <- [-1, 0, 1],
            n <- [-1, 0, 1],
            valid (x + m, y + n),
            (m, n) /= (0, 0)
        ]
      newSeat arr (x, y) =
        let nowSeat = arr A.! (x, y)
            adj = adjacents (x, y) arr
            noOccupieds = '#' `notElem` adj
         in case nowSeat of
              -- If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
              '#' -> if length (filter (== '#') adj) >= 4 then 'L' else '#'
              -- Otherwise, the seat's state does not change.
              '.' -> '.'
              -- If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
              'L' -> if noOccupieds then '#' else 'L'

      loop n arrStart arr =
        ( if arrStart == arr
            then return (n, arrStart)
            else
              loop
                (n + 1)
                arr
                ( A.listArray
                    ((0, 0), (97, 94))
                    [newSeat arr (i, j) | i <- [0 .. 97], j <- [0 .. 94]]
                )
        )

  loop 0 (A.listArray ((0, 0), (97, 94)) $ repeat '.') arrInit

pretty11 ::
  (Show a1, Num a2, Num b, Enum a2, Enum b, A.Ix a2, A.Ix b) =>
  A.Array (a2, b) a1 ->
  IO ()
pretty11 arr = forM_ [0 .. 9] $ \x ->
  print $
    take 10 $
      drop
        (10 * x)
        [arr A.! (i, j) | i <- [0 .. 9], j <- [0 .. 9]]

main12 = do
  dirs :: [(String, Int)] <-
    map (\x -> (take 1 x, read $ drop 1 x)) . lines <$> readFile' "d12.input"
  foldM moveShip (0, 0, 0) dirs

main14 =
  sum
    . (snd <$>)
    . Map.toList
    . run Map.empty []
    <$> (lines <$> readFile' "d14.input")
  where
    intToBits = reverse . toListOf bits
    charIntToBool = toEnum . read . (: [])
    run acc mask ll = case ll of
      [] -> acc
      (a : as) -> case matchTest (mkRegex "^mask") a of
        False ->
          let [mem, val] = read <$> getAllTextMatches (a =~ "[0-9]+") :: [Int]
              newVal :: Int =
                0 & partsOf (taking 64 bits)
                  .~ reverse
                    ( zipWith
                        (\m i -> if m == 'X' then i else charIntToBool m)
                        mask
                        (intToBits val)
                    )
           in run (Map.insert mem newVal acc) mask as
        True ->
          let msk = a =~ "[X,0-9]+" :: String
           in run acc (replicate 28 'X' <> msk) as

    runV2 acc mask ll = case ll of
      [] -> acc
      (a : as) -> case matchTest (mkRegex "^mask") a of
        False ->
          let [mem, val] = read <$> getAllTextMatches (a =~ "[0-9]+") :: [Int]
              addressDecoder m i = case m of
                '0' -> i
                '1' -> True
                'X' -> True -- floating
              newMem :: Int =
                0 & partsOf (taking 64 bits)
                  .~ reverse
                    (zipWith addressDecoder mask (intToBits mem))
           in run (Map.insert newMem val acc) mask as
        True ->
          let msk = a =~ "[X,0-9]+" :: String
           in run acc (replicate 28 'X' <> msk) as

main15 :: [Integer] -> IO (Integer, Integer)
main15 inp =
  return $
    go (Map.fromList $ zipWith (\i e -> (i, [e])) inp [1 ..]) 1 8
  where
    go m spoken loop =
      if loop > 30000000
        then (spoken, loop)
        else case Map.lookup spoken m of
          Just (x : y : xx) ->
            let prevDiff = x - y
             in case Map.lookup prevDiff m of
                  Nothing -> go (Map.insert prevDiff [loop] m) prevDiff (loop + 1)
                  Just x ->
                    go (Map.insert prevDiff (loop : x) m) prevDiff (loop + 1)
          Just [x] ->
            let prev = Data.Maybe.fromJust (error "nope") (Map.lookup 0 m)
             in -- case Map.lookup 0 m of
                --                       Nothing -> error "nope"
                --                       Just x -> x in
                go (Map.insert 0 (loop : prev) m) 0 (loop + 1)
          Nothing -> case Map.lookup 0 m of
            Nothing -> go (Map.insert 0 [loop] m) 0 (loop + 1)
            Just prev -> go (Map.insert 0 (loop : prev) m) 0 (loop + 1)

data Tokens = Plus | Mult | Num Int | OpenParen | CloseParen deriving (Show)

data Exp
  = Num' Int
  | Exp :+: Exp
  | --  | Exp :-: Exp
    Exp :*: Exp
  --  | Exp :/: Exp
  deriving (Show)

eval :: Exp -> Int
eval (Num' i) = i
eval (a :+: b) = eval a + eval b
eval (a :*: b) = eval a * eval b

-- eval (Div a b) = eval a `div` eval b
-- eval (Mul a b) = eval a * eval b

main16 = undefined

main17 = undefined

main18 = do
  source <- lines <$> readFile' "d18.input"
  return $ tokenize [] . words <$> source
  where
    tokenize acc = \case
      [] -> acc
      ("+" : as) -> tokenize (Plus : acc) as
      ("*" : as) -> tokenize (Mult : acc) as
      ("(" : as) -> tokenize (OpenParen : acc) as
      (")" : as) -> tokenize (CloseParen : acc) as
      (a : as) -> tokenize (Num (read a) : acc) as

    parse acc = \case
      [] -> acc
      _ -> acc

main19 = undefined

-- main20 = do
--     tiles <- parseTiles <$>  readFile' "input20-ex.txt"
--     tileDef = P.many $ P.oneOf ['#', '.']
--     tileId =  P.string "Tile " >> P.many P.digit
--     let valid C{..} = rot==180
--     let s = [1951,2311,3079,2729,1427,2473,2971,1489,1171]
--     x <- lines <$> readFile' "input20-ex.txt"
--     A.listArray ((0,0) ,(8,8)) $ unwords $ take 9 . drop 1 $ x
--     put into map of id array (9,9) Int
--     (\x -> [C a b c | a<-["90","180"], b <- [True, False], c <- x, valid (C a b c )] ) <$> (permutations s)

data Tile = Tile {ident :: Int, orientation :: Int, flipped :: Bool}

main21 = do
  recipes <- (P.parse foods "" <$>) . lines <$> readFile' "d21.input" >>= (\p -> return $ concat $ p ^.. below _Right)
  let allergenMap = foldl go Map.empty recipes
  let dangerousIngredientsList = ["ltbj", "nrfmm", "pvhcsn", "jxbnb", "chpdjkf", "jtqt", "zzkq", "jqnhd"]

  -- ltbj,nrfmm,pvhcsn,jxbnb,chpdjkf,jtqt,zzkq,jqnhd
  return $ sum $ (\Food {..} -> length $ (not . (`elem` dangerousIngredientsList)) `Prelude.filter` ingredients) <$> recipes
  where
    foods = do
      ingredients <- P.many $ P.noneOf ['(']
      Food (words ingredients) <$> P.between op cl allergenList
    op = P.char '('
    cl = P.char ')'
    comma = P.char ','
    allergenEnd = P.noneOf [',', ')']
    allergenList = P.string "contains" >> (P.space >> P.many allergenEnd) `P.sepBy` comma
    go st v = intoMap st (ingredients v) (allergens v)
    intoMap st i = \case
      [] -> st
      allergen : as -> case Map.lookup allergen st of
        Nothing -> intoMap (allergen `Map.insert` Set.fromList i $ st) i as
        Just all -> intoMap (allergen `Map.insert` Set.intersection all (Set.fromList i) $ st) i as

data Food = Food {ingredients :: [String], allergens :: [String]} deriving (Eq, Show, Ord)

main22 :: IO Int
main22 = do
  let game =
        Game
          { player1 = [39, 15, 13, 23, 12, 49, 36, 44, 8, 21, 28, 37, 40, 42, 6, 47, 2, 38, 18, 31, 20, 10, 16, 43, 5],
            player2 = [29, 26, 19, 35, 34, 4, 41, 11, 3, 50, 33, 22, 48, 7, 17, 32, 27, 45, 46, 9, 25, 30, 1, 24, 14]
          }

  let gamesamp = Game {player1 = [9, 2, 6, 3, 1], player2 = [5, 8, 4, 7, 10]}

  return $ round game
  where
    round (Game [] p2) = computeScore p2
    round (Game p1 []) = computeScore p1
    round
      (Game player1@(p : ps) player2@(q : qs)) =
        case p `compare` q of
          GT -> round (Game (ps <> [p, q]) qs)
          LT -> round (Game ps (qs <> [q, p]))
          _ -> error "cards cannot be same"
    computeScore hand = sum $ zipWith (*) (reverse hand) [1 ..]

data Game = Game {player1 :: [Int], player2 :: [Int]} deriving (Eq, Show)

main23 n = do
  let samp = asCircularList "389125467"
  let final = asCircularList "198753462"
  print $ repeatN n simulate final
  where
    simulate inp@(CL.focus -> Just fs) = rotate next3 drop3
      where
        rotate n3 (CL.rotateTo (adj (fs -1) (snd n3)) -> Just refocussed) =
          fromJust $ CL.rotateTo (fst n3) $ foldr CL.insertL refocussed (snd n3)
        rotate n3 _ =
          error "missing element for rotation"
        adj focus chosen =
          case focus `compare` 0 of
            EQ -> adj 9 chosen
            _ -> rejig chosen
              where
                rejig chosen@((focus `elem`) -> True) = adj (focus -1) chosen
                rejig _ = focus
        next3 = fromJust . uncons . reverse . take 4 . CL.rightElements . CL.rotR $ inp
        drop3 = repeatN 3 CL.removeR . CL.rotR $ inp
    simulate _ = error "cant be empty circular list"
    repeatN n = foldr (.) id . replicate n
    breakOut st el = [[el]] <> st
    asCircularList :: String -> CL.CList Int
    asCircularList = CL.fromList . (read <$>) . reverse . foldl breakOut []

main23_partii :: Int -> IO (CL.CList Int)
main23_partii n = do
  let samp = asCircularList "389125467"
  let final = asCircularList "198753462"

  let lots = [10 .. 10 ^ 6]
  return $ repeatN n simulate (CL.fromList $ [3, 8, 9, 1, 2, 5, 4, 6, 7] <> lots)
  where
    simulate inp@(CL.focus -> Just fs) = rotate next3 drop3
      where
        rotate n3 (CL.rotateTo (adj (fs -1) (snd n3)) -> Just refocussed) =
          fromJust $ CL.rotateTo (fst n3) $ foldr CL.insertL refocussed (snd n3)
        rotate n3 _ =
          error "missing element for rotation"
        adj focus chosen =
          case focus `compare` 0 of
            EQ -> adj 999999 chosen
            _ -> rejig chosen
              where
                rejig chosen@((focus `elem`) -> True) = adj (focus -1) chosen
                rejig _ = focus
        next3 = fromJust . uncons . reverse . take 4 . CL.rightElements . CL.rotR $ inp
        drop3 = repeatN 3 CL.removeR . CL.rotR $ inp
    simulate _ = error "cant be empty circular list"
    repeatN n = foldr (.) id . replicate n
    breakOut st el = [[el]] <> st
    asCircularList :: String -> CL.CList Int
    asCircularList = CL.fromList . (read <$>) . reverse . foldl breakOut []

main24 = do
  source <- (below _Right `toListOf`) . (P.parse gram "" <$>) . lines <$> readFile' "d24.input"
  let boundaries =
        foldl
          ( \(x1, y1) e -> case Map.lookup e mp of
              Just (x, y) -> (x + x1, y + y1)
              Nothing -> (x1, y1)
          )
          (0, 0)
        where
          mp =
            Map.fromList $
              zip hexDirs [(0, 1), (1, 0), (1, -1), (0, -1), (-1, 0), (-1, 1)]
  let sourceDirs = boundaries <$> concat source

  return $
    ((1 ==) . (`rem` 2) . snd)
      `filter` [(tile, length $ filter (== tile) sourceDirs) | tile <- nub sourceDirs]
  where
    hexDirs = concat $ words <$> ["e se sw w nw ne"]
    gram = P.many $ parseDirs hexDirs
    parseDirs = ap (foldr chainTry . P.try . P.string . head) tail
    chainTry = (P.<|>) . P.try . P.string

-- ans is length <$>

main25 :: IO ()
main25 = do
  let doorPublicKey = 2822615
      cardPublicKey = 1327981
      cardSubjectNumber = 7
      doorSubjectNumber = 7

      transform n loopSize res
        | (n * 7) `rem` 20201227 == res = loopSize
        | otherwise = transform ((n * 7) `rem` 20201227) (loopSize + 1) res

      encryptionKey publicKey loopSize x
        | loopSize == 0 = publicKey
        | otherwise = encryptionKey ((publicKey * x) `rem` 20201227) (loopSize -1) x

      res = (transform 1 0 doorPublicKey, transform 1 0 cardPublicKey)
      key1 = encryptionKey doorPublicKey (snd res) doorPublicKey
      key2 = encryptionKey cardPublicKey (fst res) cardPublicKey
  print (key1, key2)
  print res

moveShip current@(x, y, s) ins =
  print ("Ins:" <> show ins <> show current) >> case ins of
    ("N", v) ->
      let r = (x, y + v, s)
       in do
            print r
            return r
    ("S", v) ->
      let r = (x, y - v, s)
       in do
            print r
            return r
    ("E", v) ->
      let r = (x + v, y, s)
       in do
            print r
            return r
    ("W", v) ->
      let r = (x - v, y, s)
       in do
            print r
            return r
    ("L", v) ->
      let r = (x, y, (s + v) `mod` 360)
       in do
            print r
            return r
    ("R", v) ->
      let r =
            ( x,
              y,
              if (s - v) < 0 then (s - v + 360) `mod` 360 else (s - v) `mod` 360
            )
       in do
            print r
            return r
    ("F", v) -> case s of
      0 ->
        let r = (x + v, y, s)
         in do
              print r
              return r -- east
      90 ->
        let r = (x, y + v, s)
         in do
              print r
              return r -- north
      180 ->
        let r = (x - v, y, s)
         in do
              print r
              return r -- west
      270 ->
        let r = (x, y - v, s)
         in do
              print r
              return r -- south

getValids preamble input@(a : as) =
  if a `elem` allSubSums preamble []
    then getValids (drop 1 preamble <> [a]) as
    else return a

allSubSums inp acc = case inp of
  [] -> concat $ foldl (\st (a, b) -> ((+ a) <$> b) : st) [] acc
  _ -> allSubSums (drop 1 inp) $ (head inp, drop 1 inp) : acc

allsubseqs inp@(a : as) acc = case inp of
  [] -> Left "fooked"
  _ -> if sum acc == 16 then Right acc else allsubseqs as (a : acc)

-- 675280050

interpretb pc acc instructions =
  if pc == length instructions
    then return (acc, "TERMS")
    else case (instructions !! pc, pc) of
      (x@(Jmp n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + n)
            acc
            (take pc instructions ++ [(Jmp n, 2)] ++ drop (pc + 1) instructions)
      (x@(Jmp n, 2), _) -> return (acc, "LOOP")
      (x@(Acc n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + 1)
            (acc + n)
            (take pc instructions ++ [(Acc n, 2)] ++ drop (pc + 1) instructions)
      (x@(Acc n, 2), _) -> return (acc, "LOOP")
      (x@(Nop n, 1), _) ->
        print (pc, acc, x)
          >> interpret
            (pc + 1)
            acc
            (take pc instructions ++ [(Nop n, 2)] ++ drop (pc + 1) instructions)
      ((Nop n, 2), _) -> return (acc, "LOOP")

parseOperand op = case words op of
  "jmp" : v : _ -> makeOp Jmp v
  "acc" : v : _ -> makeOp Acc v
  "nop" : v : _ -> makeOp Nop v

makeOp :: (Int -> Ins) -> String -> Ins
makeOp op v = case head v of
  '+' -> op (read (drop 1 v) :: Int)
  _ -> op (read v :: Int)

data Ins = Nop Int | Acc Int | Jmp Int deriving (Show)

getIntersects (l : ls) = foldl intersect l ls

getIds =
  (sort expected ==) . sort
    . filter (/= "cid")
    . Map.keys
    . Map.fromList
    . map (break (== ':'))

for = flip map

getId4b inputList =
  let passportFields = Map.fromList $ for inputList (break (== ':'))
   in ( (sort expected ==) . sort $
          filter (/= "cid") $
            Map.keys
              passportFields,
        passportFields
      )

expected = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

parseFile = foldr newLineDemarcated []
  where
    newLineDemarcated e st = case e of
      "" -> st <> "\n"
      _ -> st <> " " <> e

-- newLineDemarcated "" st = st <> "\n"
-- newLineDemarcated _  st = st <> " " <> e

trimmed = drop 1

validateYear st end y
  | matchTest numbs (trimmed y) =
    let psd = read y :: Int in (psd >= st && psd <= end)
validateYear _ _ _ = False

trimEnd = reverse . drop 2 . reverse

validateH st end y =
  let psd = read (trimEnd y) :: Int in (psd >= st && psd <= end)

numbs = mkRegex "^:[0-9]{4}$"

hgtIn = mkRegex "^[0-9]+in$"

hgtCm = mkRegex "^[0-9]+cm$"

hairC = mkRegex "#[0-9,[a-f]{6}"

mkRegex = makeRegexOpts defaultCompOpt {multiline = False} defaultExecOpt

validateMap :: Map.Map String String -> [Bool]
validateMap = Map.foldrWithKey validatePassport []
  where
    validatePassport "byr" v acc = validateYear 1920 2002 v : acc
    validatePassport "iyr" v acc = validateYear 2010 2020 v : acc
    validatePassport "eyr" v acc = validateYear 2020 2030 v : acc
    validatePassport "hgt" v acc | matchTest hgtIn v = validateH 150 193 v : acc
    validatePassport "hgt" v acc | matchTest hgtCm v = validateH 59 76 v : acc
    validatePassport "hcl" v acc = matchTest hairC v : acc
    -- validatePassport "ecl" v acc = acc
    -- validatePassport "pid" v acc = acc

    validatePassport _ v acc = acc

-- sequence [
-- main,
-- main2,
-- main3,
-- main4,
-- main4b,
-- main6a,
-- main6b,
-- main8,
-- main8b,
-- main9,
-- main10,
-- main11,
-- main12,
-- main14,
-- main15,
-- main15,
-- main16,
-- main17,
-- main18,
-- main19,
-- main21,
-- main22,
-- main22,
-- main23,
-- main23_partii,
-- main23_partii,
-- main24,
-- main25,
-- main25,

-- ]
--