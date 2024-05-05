module AlphaPos where
import Data.List
import Data.Maybe
import Data.Char

coordinateList :: [a] -> [b] -> [(a,b)]
coordinateList [] _ = []
coordinateList _ [] = []
coordinateList (a:as) (b:bs) = [(a, b)] ++ coordinateList as bs

alphaKeyf = coordinateList [1..26] ['a'..'z']
alphaKeyb = (coordinateList ['a'..'z'] (map show [1..26])) ++ [(' ', "27")]


encoderLetToPos :: Char -> String
encoderLetToPos char
    | char == ' '           = []
    | elem (toLower char) ['a'..'z']  = snd (alphaKeyb !! match)
    | otherwise             = []
    where match = fromMaybe (27) (elemIndex (toLower char) (map fst alphaKeyb))


--pre = map encoderLetToPos

encodeMap str = concat (intersperse " " (spaceFilter conversionIntermediate))
    where
        spaceFilter = filter (/="")
        conversionIntermediate = map encoderLetToPos str
