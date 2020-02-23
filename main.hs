import Data.Hashable
import Data.Sequence
import Data.Sequences
import Data.Word
import qualified Data.Sequence as Sequence
import Data.Digest.Murmur64

data BloomFilter = BloomFilter {
  seeds :: [Int],
  vector :: Seq Bool
}

initialWords = ["Example", "words", "in", "filter"]
initialFilter = BloomFilter {
  seeds = [1, 2, 3],
  vector = Sequence.replicate 50 False }
bloomFilter = foldr (\word bf -> computeNewFilter bf word) initialFilter initialWords

main = do
  str <- getLine
  () <- putStrLn $ boolToString $ checkFilter bloomFilter str
  main

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

-- | Is the given string in the filter?
checkFilter :: BloomFilter -> String -> Bool
checkFilter bf str =
  let
    s = seeds bf
    vec = vector bf
    hashIndexes = getHashIndexes bf str
  in and $ map (\idx -> indexEx vec idx) hashIndexes

-- | Compute a new bit vector for the given string, with a list of hash seeds
computeNewFilter :: BloomFilter -> String -> BloomFilter
computeNewFilter bf [] = bf
computeNewFilter bf str =
  let
    s = seeds bf
    vec = vector bf
    hashIndexes = getHashIndexes bf str
  in BloomFilter { seeds = s, vector = foldr updateSeq vec hashIndexes }

-- | Get the hash indexes for a string
getHashIndexes :: BloomFilter -> String -> [Int]
getHashIndexes bf str =
  let
    s = seeds bf
    vec = vector bf
    hashValues = map (\hash -> getHashValue hash str) s
  in map (\val -> mod (fromIntegral val) $ Sequence.length vec) hashValues

-- | Get the hash value for a string, with the given seed
getHashValue :: Int -> String -> Word64
getHashValue seed str = asWord64 $ hash64WithSeed (fromIntegral seed) str 

-- | Update the bit vector with the given indexes
updateSeq :: Int -> Seq Bool -> Seq Bool
updateSeq idx vec = update idx True vec