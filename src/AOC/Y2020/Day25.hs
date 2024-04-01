module AOC.Y2020.Day25 where

import Library

main :: IO ()
main = do
  let doorPublicKey = 2822615
      cardPublicKey = 1327981
      cardSubjectNumber = 7
      doorSubjectNumber = 7

      transform n loopSize res
        | (n * 7) `rem` 20201227 == res = loopSize
        | otherwise = transform ((n * 7) `rem` 20201227) (loopSize + 1) res

      encryptionKey publicKey loopSize x
        | loopSize == 0 = publicKey
        | otherwise =
            encryptionKey
              ((publicKey * x) `rem` 20201227)
              (loopSize - 1)
              x

      res = (transform 1 0 doorPublicKey, transform 1 0 cardPublicKey)
      key1 = encryptionKey doorPublicKey (snd res) doorPublicKey
      key2 = encryptionKey cardPublicKey (fst res) cardPublicKey
  print (key1, key2)
  print res
