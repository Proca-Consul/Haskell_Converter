
import Data.List

converterDec :: Int -> (String, String, String)
converterDec n 
    = ("Binary: " ++ bin, "Octal: " ++ oct, "Hexadecimal: " ++ hex)
  where 
    bin = show(binary n)
    oct = show(octal n)
    hex = fromBinToHex bin

converterBin :: Int -> (String, String, String)
converterBin n 
    = ("Decimal: " ++ dec, "Octal: " ++ oct, "Hexadecimal: " ++ hex)
  where 
    dec = show(fromBinToDec n)
    oct = show(fromBinToOct n)
    hex = fromBinToHex (show n)

-- Basic Conversion: Binary/Octal/Hexadecimal -------------------

binary :: Int -> Int
binary n = newbase n 2

octal :: Int -> Int
octal n = newbase n 8

fromBinToHex :: String -> String
fromBinToHex "" = ""
fromBinToHex b  = lookUp hexMap fourDig ++ fromBinToHex rest
  where
   (fourDig, rest) = splitAt 4 (make4 b)

fromBinToDec :: Int -> Int
fromBinToDec 0 = 0
fromBinToDec n = sum (zipWith (*) binList powOfTwo)
  where
    binList  = toList n
    powOfTwo = reverse (map (2^) [0.. digits - 1])
    digits   = countDigits n

fromBinToOct :: Int -> Int
fromBinToOct b
  | (length . toList) b <= 3 = fromBinToDec threeDig
  | otherwise                
       = fromBinToDec threeDig * 10^((length(make3(toList b)) `div` 3) - 1) + fromBinToOct rest
  where
    (three, tail) = splitAt 3 (make3(toList b))
    threeDig      = fromList three
    rest          = fromList tail     

-- Helper functions ---------------------------------------------

fromList :: [Int] -> Int 
fromList []       = 0
fromList (n : ns) = n * 10^(length ns) + fromList ns

toList :: Int -> [Int]
toList n
  | countDigits n == 1 = [n]
  | otherwise          = toList (n `div` 10) ++ [(n `mod` 10)]

countDigits :: Int -> Int 
countDigits 0 = 0
countDigits n = 1 + countDigits (n `div` 10)

-- Adjust binary string to 4/3 digits format 

make4 :: String -> String
make4 str 
  | (length str) `mod` 4 == 0 = str
  | otherwise                 = make4 ("0" ++ str)

make3 :: [Int] -> [Int]
make3 list
  | (length list) `mod` 3 == 0 = list
  | otherwise                  = make3 (0 : list)

-- Convert any decimal number to given base

newbase :: Int -> Int -> Int 
newbase n b 
	| n < b     = n 
	| otherwise = (newbase (n `div` b) b) * 10 + n `mod` b

-- Hexadecimal - Binary Map--------------------------------------

hexMap :: [(String, String)]
hexMap = [("0", "0000"), ("1", "0001"), ("2", "0010"), ("3", "0011"),
          ("4", "0100"), ("5", "0101"), ("6", "0110"), ("7", "0111"),
          ("8", "1000"), ("9", "1001"), ("A", "1010"), ("B", "1011"),
          ("C", "1100"), ("D", "1101"), ("E", "1110"), ("F", "1111")]

lookUp :: Eq a => [(a, a)] -> a -> a
lookUp pairs key = head [ a | (a, b) <- pairs, b == key ]

lookUpRev :: Eq a => [(a, a)] -> a -> a
lookUpRev pairs key = head [ b | (a, b) <- pairs, a == key ]

