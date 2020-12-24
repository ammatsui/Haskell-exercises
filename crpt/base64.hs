import Data.Char (ord, chr)
import Data.Array


-- convert decimal to binary
helper :: Int -> [Int]
helper 0 = []
helper n = let (q,r) = n `divMod` 2 in r : helper q

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse (helper n)

-- n-bit binary
toN :: Int -> [Int] -> [Int]
toN n xs = if length xs == n then xs else toN n (0 : xs) 

-- "join together"
join s = foldl (++) [] s

-- groups of 6
blocks [] _ = []
blocks s  n = (take n s) : blocks (drop n s) n  

-- from binary to decimal
toDec :: [Int] -> Int
toDec []     = 0
toDec (x:xs) = x*2^i + toDec xs where i = length xs

-------------------------------------------------------------------------

-- base64 index table
encodeArray :: Array Int Char
encodeArray = array (0,64) 
          [ (0,'A'),  (1,'B'),  (2,'C'),  (3,'D'),  (4,'E'),  (5,'F')                    
          , (6,'G'),  (7,'H'),  (8,'I'),  (9,'J'),  (10,'K'), (11,'L')                    
          , (12,'M'), (13,'N'), (14,'O'), (15,'P'), (16,'Q'), (17,'R')
          , (18,'S'), (19,'T'), (20,'U'), (21,'V'), (22,'W'), (23,'X')
          , (24,'Y'), (25,'Z'), (26,'a'), (27,'b'), (28,'c'), (29,'d')
          , (30,'e'), (31,'f'), (32,'g'), (33,'h'), (34,'i'), (35,'j')
          , (36,'k'), (37,'l'), (38,'m'), (39,'n'), (40,'o'), (41,'p')
          , (42,'q'), (43,'r'), (44,'s'), (45,'t'), (46,'u'), (47,'v')
          , (48,'w'), (49,'x'), (50,'y'), (51,'z'), (52,'0'), (53,'1')
          , (54,'2'), (55,'3'), (56,'4'), (57,'5'), (58,'6'), (59,'7')
          , (60,'8'), (61,'9'), (62,'+'), (63,'/') ]

-- from the input string to a 24-bit string
strTo :: String -> [Int]
strTo s = join $ map (toN 8 . toBin) (map ord s)

-- from a 24-bit string to the list of final integers
toInd :: [Int] -> [Int]
toInd s = map toDec ss where ss = blocks s 6

-- get element from index
getEl :: Int -> Char
getEl i = (!) encodeArray i

encode :: String -> String
encode s = map getEl $ (toInd . strTo) s

-- output padding
--pad = undefined

----------------------------------------------------------------------------------------

-- encrypted string to integers
dcd :: String -> [Int]
dcd [] = []
dcd (h:t)
    | h <= 'Z' && h >= 'A'  =  ord h - ord 'A'      : dcd t
    | h >= '0' && h <= '9'  =  ord h - ord '0' + 52 : dcd t
    | h >= 'a' && h <= 'z'  =  ord h - ord 'a' + 26 : dcd t
    | h == '+'  = 62 : dcd t
    | h == '/'  = 63 : dcd t
    | h == '='  = []  
    | otherwise = dcd t


-- from the encrypted string to a 24-bit string
enStrTo :: String -> [Int]
enStrTo s = join $ map (toN 6 . toBin) (dcd s)

-- from a 24-bit to ascii values
getInd :: [Int] -> [Int]
getInd s = map toDec (blocks s 8)

decode :: String -> String
decode s = map chr $ (getInd . enStrTo) s