module Codewars.BuildTower where


buildTower :: Int -> [String]
buildTower n = reverse $ buildTower' oddnumber 0
  where
    oddnumber = nthOddNumber n 
  
  
buildTower' :: Int -> Int -> [String]
buildTower' (-1) _ = []
buildTower' n acc   = [replicate acc ' '  ++ replicate n '*' ++ replicate acc ' '] ++ buildTower' (n + (-2)) (acc + 1) 
  

nthOddNumber :: Int -> Int
nthOddNumber n = head . take 1 $ drop (n + (-1)) [1,3..]

--n = 4
--1|...*...  (n-3) '.' 2(n-4) + 1 '*' (n-1) '.' 
--3|..***..   (n-1) '.' 2(n-3) + 1 '*' (n-1) '.' 
--3|.*****.  2(n-2) + 1 '*'
--4|*******  2(n - 1) + 1 *  aka 4th odd num

--n = 3
--1|..*..  (n-3) '.' (n-2) '*' (n-1) '.' 
--3|.***.   (n-1) '.' (n) '*' (n-1) '.' 
--3|*****  (n+2) '*'

--n = 2
--1|.*. (n-1) '.' (n-1) '*' (n-1) '.'
--2|*** (n+1) *
