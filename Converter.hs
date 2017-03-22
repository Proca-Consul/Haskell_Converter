
-- Copyright: Ivan Procaccini© 2015
-- To W. L., because "it's a bit like when you are childven,
-- and you play wiph your toys: ah ah you control your toys
-- but but but your parents... ahh, they control you".

import Data.List
import Data.List.Split
import Data.Char

converterDec :: Int -> String
converterDec n 
  = "> " ++ "Binary: " ++ bin ++ "\n> " ++ "Octal: " ++ oct ++
    "\n> " ++  "Hexadecimal: " ++ hex
  where 
    bin = fromListToString(binary n)
    oct = fromListToString(octal n)
    hex = fromBinToHex bin

converterB :: String -> String
converterB n 
  = "> " ++ "Decimal: " ++ dec ++ "\n> " ++ "Octal: " ++ oct ++
    "\n> " ++  "Hexadecimal: " ++ hex
  where 
    dec' = fromBinToDec n
    dec = show(fromBinToDec n)
    oct = fromListToString(octal dec')
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
  putStrLn "> NOTE: the conversion is only suitable for unsigned integers up to 2^63 - 1"
  loop 

loop :: IO()
loop = do
  putStrLn "> Enter command [quit | bin <Number> | dec <Number>]:"
  putStr "> "
  input <- getLine
  conv (head(splitOn " " input)) (head(tail(splitOn " " input)))   
{-   where -}
    {- mode = head((splitOn " " input))   -}
    {- number = tail((splitOn " " input)) -}

conv :: String -> String -> IO()
conv "quit" _ = return()
conv "bin" number = checkFromBin number 
conv "dec" number = checkFromDec number
conv _ _ = putStrLn "> Invalid command." >> loop

checkFromDec :: String -> IO()
checkFromDec number 
  | not (all isDigit number) 
      = putStrLn "> Invalid input: non-numeric." >> loop
  | otherwise
      = putStrLn (converterDec (read number :: Int)) >> loop 

checkFromBin :: String -> IO()
checkFromBin number 
  | not (all isDigit number) 
      = putStrLn "> Invalid input: non-numeric." >> loop
  | filter (\ d -> d /= '1' && d /= '0') number /= [] || 
    (length number) > 63 
      = putStrLn "> Invalid Binary Number." >> loop
  | otherwise
      = putStrLn (converterB number) >> loop

  

