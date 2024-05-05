module AlphaPosition where
import Data.List
import Data.Maybe
import Data.Char

--Code Makes Enconding pairs by an input list [a] and output list [b]
coordinateList :: [a] -> [b] -> [(a,b)]
coordinateList [] _ = []
coordinateList _ [] = []
coordinateList (a:as) (b:bs) = [(a, b)] ++ coordinateList as bs

--UNUSED
--list in alphaKeyb but opposite [b] [a]
--alphaKeyf = coordinateList [1..26] ['a'..'z']

-- Makes list [(Letter, Position)]
-- example element : (a, 1)
alphaKeyStrings = coordinateList ['a'..'z'] $ map  show  [1..26]
alphaKeyNums = coordinateList ['a'..'z']  [1..26]

-- Function looks at a single character and returns its alphabet position as a string
encoderLetToPos :: Char -> String
encoderLetToPos char = snd $ alphaKeyStrings !! matchedLetter
    where matchedLetter = fromJust $ elemIndex (toLower char) (map fst alphaKeyStrings)

-- Function looks at a position and returns its corresponding alphabetposition 
encoderPosToLet :: Int -> Char
encoderPosToLet int = fst $ alphaKeyNums !! matchedPos
    where matchedPos = fromJust $ elemIndex (int) (map snd alphaKeyNums)

--Function does the following
-- 1) "flattens" string to eliminate case sensitivity
-- 2) filters out any non english alphabet characters
-- 3) maps the encoder to the filtered string
-- 4) concatenates string adding spaces between position chunks
alphabetPosition :: (Char -> String) -> String -> String
alphabetPosition func str = unwords . map func . filter (\x -> x `elem` ['a'..'z']) $ map toLower str
  --where encoder = func

--Code takes alphabet base key, and changes key to a new mapping
-- of the form [(a, c)]
keyMap :: (b -> c) -> [(a, b)] -> [(a, c)]
keyMap f list = coordinateList (map fst list) newList
  where newList = map f (map snd list) 


-- Prove that f(x) = n + x is bijective in z26 to z26 
-- but for now...
-- this function simply shifts a number in the mod space
cesarShiftAdd :: Integral a => a -> a -> a
cesarShiftAdd a b = 1 + mod (a + (b - 1)) 26

-- generates a function mapping for a cesar key 
cesarShiftAddKey :: (Enum a, Integral a) => a -> [(Char, a)] 
cesarShiftAddKey pkey =  coordinateList ['a'..'z'] $ map (\x -> cesarShiftAdd pkey x) [1..26] 


--Same as above, but maps alphabet letter to letter
cesarKeyChars :: Int -> [(Char, Char)]
cesarKeyChars pkey = keyMap (encoderPosToLet) $ cesarShiftAddKey pkey


--cesarShiftAddCharVersion pkey = coordinateList (['a'..'z']) (map snd $ cesar)



--keyIntToStr ::(b -> c) -> [(a, b)] -> [(a, c)] 
--keyIntToStr func [] = []
--keyIntToStr func ((a, b):rest) = [(a, func b)] ++ keyIntToStr func rest

encoderLetToAny :: ([(Char, Int)]) -> Char -> String 
encoderLetToAny key char = show $ snd $ key !! matchedLetter
    where matchedLetter = fromJust $ elemIndex (toLower char) ( map fst key)


-- this takes a function mapping of chars to chars,
-- and an input, and generates an output based on the ordered pairs
encoderGeneralSafe :: [(Char, Char)] -> Char -> Char
encoderGeneralSafe key char
  | elem char ['a'..'z'] = snd $ key !! matchedLetter
  | otherwise            = char
  where
    matchedLetter = fromJust $ elemIndex (toLower char) (map fst key)


--used for quick testing
cesar1 = cesarKeyChars 1


-- This code takes a private key and encodes a string with an
-- integer shift in the cesar cipher style
cesarEncodeString :: Int -> String -> String
cesarEncodeString pkey plaintext = map  (encoderGeneralSafe (cesarKeyChars pkey)) str
  where str = map toLower plaintext

-- Run code wiht int argument 25 to loop through various
-- cesar cipher shifts
decodeCesar :: Int -> String -> [String]
decodeCesar 0 cipherText = [cipherText]
decodeCesar num cipherText =  decodeCesar (num - 1) cipherText ++ [cesarEncodeString (- num) cipherText] 

-- used as a sample for decoding
sampleCesar = cesarEncodeString 5 "Hello there"  



