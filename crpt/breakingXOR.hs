import Data.Bits (xor)
import Data.Char
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Data.Text.Encoding (encodeUtf8)
import Data.List as L
import Data.Function (on)
import Data.Maybe
import Data.Functor
import Control.Applicative




-- XORres 2 strings
stringXor :: String -> String -> String
stringXor s t = map chr $ zipWith xor (map ord s) (map ord t)

-- encryption and decryption
encDec :: String -> String -> String
encDec text key = stringXor (take (length text) (cycle key)) text


---------------------------------------------- single-byte xor cypher --------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
 
keys :: [String]
keys = foldr (\ n xs -> [chr n] : xs) [] (take 256 [0..])


-- evil laughter single byte
attack :: String -> [String]
attack text = map (encDec text) keys

ok = ['A'..'Z'] ++ ['a'..'z'] ++ " " ++ "\"!.,\\?-'=;:%"

isOk :: String -> Bool
isOk s = all (\c -> elem c ok) s

modify :: String -> String
modify s = foldr (\c s -> case c of
                          '\"' -> "\\"++"\"" ++ s
                          '\'' -> "\\"++"\'" ++ s
                          '\\' -> "\\"++"\\" ++ s
                          otherwise -> c:s)           [] s

-- breaking single-byte xor cypher
main = do
      putStrLn "Which text are we attacking?"
      encr <- getLine
      let text = read ('"' : (modify encr) ++ "\"") :: String in let list = attack text in return $ foldr (\s ss -> if isOk (fst s) then s:ss else ss) [] (zip list keys)

----------------------------------------------- repeating key xor cypher -------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------- guessing the key length -----------------------------------------------------
-- break text into blocks of equal length

blocks :: String -> Int -> [String]
blocks [] n = []
blocks s  n = (take n s) : blocks (drop n s) n 

-- convert strings into byte arrays
-- then convert bytes into bits
-- then calculate the differences

--unpack :: ByteString -> [Word8] 

packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack

helper 0 = []
helper n = let (q,r) = n `divMod` 2 in r : helper q

toBin 0 = [0]
toBin n = reverse (helper n)

toInt s = map (\n -> read n :: Int) (map show s)

-- all possible combinations
combs :: Int -> [a] -> [[a]]
combs _ []     = []
combs 0 _      = []
combs 1 x      = L.map (:[]) x
combs n (x:xs) = (L.map (x:) (combs (n-1) xs) ) ++ combs n xs   

-- normalised (by keysize) hamming distance (the legnth of the shorter string)
nHamming :: String -> String -> Double
nHamming s t = (fromIntegral (length $ filter (\c -> c == 1) $ concat $ map toBin . toInt $ zipWith xor (B.unpack (packStr s)) (B.unpack (packStr t)))) / (minLength s t) where minLength s t = fromIntegral $ min (length s) (length t)

-- returns list of normalised (by keysize) hamming distances for all possible pairs of blocks for a given keysize (which is the length of blocks)
allHam :: [String] -> [Double]
allHam ss = map (\[x, y] -> nHamming x y) (combs 2 ss)

-- calculates the average = sum of hamming distances / amount
avHam :: [String] -> Double
avHam []  = 0.0
avHam [x] = 0.0
avHam s   = 2* (sum (allHam s)) / (m * (m - 1))  where m = (fromIntegral (length s))

keyLengths s = let n = floor $ (fromIntegral (length s))/2.0 in take n [1..]

posKeys :: String -> [(Double, Int)]
posKeys s = L.sortBy (on compare fst) $ zip (map avHam (map (blocks s) (keyLengths s))) [1..]

----------------------------------------------------- guess the key knowing its length ------------------------------------------
--mattack :: Int -> String -> [String] foldr (\s ss -> if isOk s then s:ss else ss) [] $


-- n is the key length
list s n = map attack $ transpose (blocks s n) 

-- so that I can see the keys
listK s n = map (\s -> zip s keys) (list s n)

-- leave only the beautiful ones
bList s n = map (foldr (\t ss -> if (isOk (fst t)) then t:ss else ss) []) (listK s n)

-- candidates for the corresponding position in the actual key
keysPos s n = map (\t -> map snd t) (bList s n)


------------------------------------------------------ from hex ----------------------------------------------------------------------

hexChar :: Char -> Int
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | otherwise = 0

parseHex :: String -> Int
parseHex hxStr 
    | length hxStr /= 0 = (hexChar(last(hxStr)))+(16*parseHex(init(hxStr)))
    | otherwise         = 0   

fromHex :: String -> String
fromHex s = map chr $ map parseHex (blocks s 2)
