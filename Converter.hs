
-- Copyright: Ivan Procaccini© 2015
-- To Prof. Wayne Luk, because "it's a bit when you are chilven,
-- and you play wiph your toys: ah ah you control your toys
-- but but but your parents... ahh they control you".

import Data.List
import Data.Char

converterDec :: Int -> String
converterDec n 
    = "\n" ++ "Binary: " ++ bin ++ "\n" ++ "Octal: " ++ oct ++
      "\n" ++  "Hexadecimal: " ++ hex ++ "\n"
  where 
    bin = fromListToString(binary n)
    oct = fromListToString(octal n)
    hex = fromBinToHex bin

converterBin :: (Num a, Show a) => a -> String
converterBin n = converterB (show n)

converterB :: String -> String
converterB n 
    = "\n" ++ "Decimal: " ++ dec ++ "\n" ++ "Octal: " ++ oct ++
      "\n" ++  "Hexadecimal: " ++ hex ++ "\n"
  where 
    dec = show(fromBinToDec n)
    oct = show(fromBinToOct n)
    hex = fromBinToHex n

-- Basic Conversion: Binary/Octal/Hexadecimal -------------------

binary :: Int -> [Int]
binary n = newbase n 2

octal :: Int -> [Int]
octal n = newbase n 8

fromBinToHex :: String -> String
fromBinToHex "" = ""
fromBinToHex b  = lookUp hexMap fourDig ++ fromBinToHex rest
  where
   (fourDig, rest) = splitAt 4 (make4 b)

fromBinToDec :: String -> Int
fromBinToDec s
  | not(elem '1' s) = 0 
  | otherwise  = sum (zipWith (*) binList powOfTwo)
  where
    binList  = toList s
    powOfTwo = reverse (map (2^) [0.. digits - 1])
    digits   = length s

fromBinToOct :: String -> Int
fromBinToOct b
  | (length . toList) b <= 3 = toDec3
  | otherwise                
       = toDec3 * 10^((length(make3(toList b)) `div` 3) - 1) + toDecT
  where
    (three, tail) = splitAt 3 (make3(toList b))
    threeStr      = fromListToString three
    tailStr       = fromListToString tail
    toDec3        = fromBinToDec threeStr
    toDecT        = fromBinToDec tailStr

-- Helper functions ---------------------------------------------

fromList :: [Int] -> Int 
fromList []       = 0
fromList (n : ns) = n * 10^(length ns) + fromList ns

toList :: String -> [Int]
toList s
  | length  s == 1 = [digitToInt (head s)]
  | otherwise      = (digitToInt n) : toList ns
  where 
   (n : ns) = s

fromListToString :: [Int] -> String
fromListToString []       = ""
fromListToString (n : ns) = show(n) ++ fromListToString ns


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

newbase :: Int -> Int -> [Int]
newbase n b
  | n < b = [n]
  | otherwise = (newbase (n `div` b) b) ++ [n `mod` b]

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

-- Main ---------------------------------------------------------

converter :: IO ()
converter = do
  putStrLn ""
  putStrLn "    #####################"
  putStrLn "    #                   #"
  putStrLn "    # HA∑KEΛΛ Converter #"
  putStrLn "    #                   #"
  putStrLn "    #####################"
  putStrLn ""
  putStrLn "> Instructions:"
  putStrLn "> Type [1] for Dec -> (Bin, Oct, Hex) conversion"
  putStrLn "> Type [2] for Bin -> (Dec, Oct, Hex) conversion"
  putStrLn "> NOTE: The conversion is suitable for unsigned integers up to 2^63 - 1"
  mode <- getLine
  putStrLn "> Enter the number to convert:"
  input <- getLine
  putStrLn (conv mode input)
  putStrLn "> Exiting. Bzzz..."
  return ()
  
conv :: String -> String -> String
conv mode input
  | ((read mode :: Int) == 1) 
      = converterDec (read input :: Int)
  | (length input) > 63 
      = "> Invalid Binary Number"
  | filter (\ d -> d /= '1' && d /= '0') input /= [] 
      = "> Invalid Binary Number"
  | otherwise 
      = converterB input
  
          

  
       
