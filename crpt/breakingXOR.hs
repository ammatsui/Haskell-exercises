{-# LANGUAGE ViewPatterns #-}   

import Data.Bits (xor)
import Data.Char
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Data.Text.Encoding (encodeUtf8)
import Data.List as L
import Data.Function (on)
import Data.Maybe






-- XORres 2 strings
stringXor :: String -> String -> String
stringXor s t = map chr $ zipWith xor (map ord s) (map ord t)

-- encryption and decryption
encDec :: String -> String -> String
encDec text key = stringXor (take (length text) (cycle key)) text

keys :: [String]
keys = foldr (\ n xs -> [chr n] : xs) [] (take 256 [0..])


-- evil laughter single byte
attack :: String -> [String]
attack text = map (encDec text) keys

ok = ['A'..'Z'] ++ ['a'..'z'] ++ " " ++ "\"!.,\\?-\'=;:%"

isOk :: String -> Bool
isOk s = all (\c -> elem c ok) s

modify :: String -> String
modify s = foldr (\c s -> case c of
                          '\"' -> "\\"++"\"" ++ s
                          '\'' -> "\\"++"\'" ++ s
                          '\\' -> "\\"++"\\" ++ s
                          otherwise -> c:s)           [] s


main = do
      putStrLn "Which text are we attacking?"
      encr <- getLine
      let text = read ('"' : (modify encr) ++ "\"") :: String in let list = attack text in return $ foldr (\s ss -> if isOk (fst s) then s:ss else ss) [] (zip list keys)




-- break text into blocks of equal length
-- those should be blocks with n bits length
-- string -> bytestring -> ~crop~ -> [bytestring] -> [string]

blocks' :: String -> Int -> [String]
blocks' s n = (take n s) : blocks' (drop n s) n 

blocks :: String -> Int -> [String]
blocks [] n = []
blocks s  n =  map C.unpack $ (B.take n s') : map packStr (blocks (C.unpack (B.drop n s')) n) where s' = packStr s                                                   

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

combs :: Int -> [a] -> [[a]]
combs _ []     = []
combs 0 _      = []
combs 1 x      = L.map (:[]) x
combs n (x:xs) = (L.map (x:) (combs (n-1) xs) ) ++ combs n xs   

-- normalised (by keysize) hamming distance
nHamming :: String -> String -> Double
nHamming s t = (fromIntegral (length $ filter (\c -> c == 1) $ concat $ map toBin . toInt $ zipWith xor (B.unpack (packStr s)) (B.unpack (packStr t)))) / (minLength s t) where minLength s t = fromIntegral $ min (length s) (length t)

-- returns list of normalised (by keysize) hamming distances for all possible pairs of blocks for a given keysize (which is the length of blocks)
alham :: [String] -> [Double]
alham ss = map (\[x, y] -> nHamming x y) (combs 2 ss)

-- calculates the average = sum of hamming distances / amount
edham :: [String] -> Double
edham [] = 0.0
edham s  = (sum (alham s)) / (m * (m -1))  where m = (fromIntegral (length s))


keyLengths s = let n = floor $ (fromIntegral (length s))/2.0 in take n [1..]

posKeys :: String -> [(Double, Int)]
posKeys s = L.sortBy (on compare fst) $ zip (map edham (map (blocks s) (keyLengths s))) [1..]

--mattack :: Int -> String -> [String] foldr (\s ss -> if isOk s then s:ss else ss) [] $


-- n is the key length
list s n = map attack $ transpose (blocks s n) 

-- so that I can see keys
listK s n = map (\s -> zip s keys) (list s n)

-- leave only the beautiful ones
bList s n = map (foldr (\t ss -> if (isOk (fst t)) then t:ss else ss) []) (listK s n)

-- candidates for the corresponding position in the actual key
keysPos s n = map (\t -> map snd t) (bList s n)


------------------------------------------------------


fromHex :: String -> String
fromHex s = concat $ map frmHx (blocks s 2)

frmHx :: String -> String
frmHx s = case s of
          "00" -> "\NUL"
          "01" -> "\SOH"
          "02" -> "\STX"
          "03" -> "\ETX"
          "04" -> "\EOT"
          "05" -> "\ENQ"
          "06" -> "\ACK"
          "07" -> "\BEL"
          "08" -> "\BS"
          "09" -> "\HT"
          "0A" -> "\LF"
          "0B" -> "\VT"
          "0C" -> "\FF"
          "0D" -> "\CR"
          "0E" -> "\SO"
          "0F" -> "\SI"
          "10" -> "\DLE"
          "11" -> "\DC1"
          "12" -> "\DC2"
          "13" -> "\DC3"
          "14" -> "\DC4"
          "15" -> "\NAK"
          "16" -> "\SYN"
          "17" -> "\ETB"
          "18" -> "\CAN"
          "19" -> "\EM"
          "1A" -> "\SUB"
          "1B" -> "\ESC"
          "1C" -> "\FS"
          "1D" -> "\GS"
          "1E" -> "\RS"
          "1F" -> "\US"
          "20" -> " "
          "21" -> "!"
          "22" -> "\""
          "23" -> "#"
          "24" -> "$"
          "25" -> "%"
          "26" -> "&"
          "27" -> "'"
          "28" -> "("
          "29" -> ")"
          "2A" -> "*"
          "2B" -> "+"
          "2C" -> ","
          "2D" -> "-"
          "2E" -> "."
          "2F" -> "/"
          "30" -> "0"
          "31" -> "1"
          "32" -> "2"
          "33" -> "3"
          "34" -> "4"
          "35" -> "5"
          "36" -> "6"
          "37" -> "7"
          "38" -> "8"
          "39" -> "9"
          "3A" -> ":"
          "3B" -> ";"
          "3C" -> "<"
          "3D" -> "="
          "3E" -> ">"
          "3F" -> "?"
          "40" -> "@"
          "41" -> "A"
          "42" -> "B"
          "43" -> "C"
          "44" -> "D"
          "45" -> "E"
          "46" -> "F"
          "47" -> "G"
          "48" -> "H"
          "49" -> "I"
          "4A" -> "J"
          "4B" -> "K"
          "4C" -> "L"
          "4D" -> "M"
          "4E" -> "N"
          "4F" -> "O"
          "50" -> "P"
          "51" -> "Q"
          "52" -> "R"
          "53" -> "S"
          "54" -> "T"
          "55" -> "U"
          "56" -> "V"
          "57" -> "W"
          "58" -> "X"
          "59" -> "Y"
          "5A" -> "Z"
          "5B" -> "["
          "5C" -> "\\"
          "5D" -> "]"
          "5E" -> "^"
          "5F" -> "_"
          "60" -> "`"
          "61" -> "a"
          "62" -> "b"
          "63" -> "c"
          "64" -> "d"
          "65" -> "e"
          "66" -> "f"
          "67" -> "g"
          "68" -> "h"
          "69" -> "i"
          "6A" -> "j"
          "6B" -> "k"
          "6C" -> "l"
          "6D" -> "m"
          "6E" -> "n"
          "6F" -> "o"
          "70" -> "p"
          "71" -> "q"
          "72" -> "r"
          "73" -> "s"
          "74" -> "t"
          "75" -> "u"
          "76" -> "v"
          "77" -> "w"
          "78" -> "x"
          "79" -> "y"
          "7A" -> "z"
          "7B" -> "{"
          "7C" -> "|"
          "7D" -> "}"
          "7E" -> "~"
          "7F" -> "\DEL"
          otherwise -> ""