module CreatePhoneNumber where

import Data.Char

createPhoneNumber :: [Int] -> String
createPhoneNumber nums = formatter . concat. map show $ nums
  where 
    formatter (a:b:c:d:e:f:g:h:i:j:emptys) =
      '(':a:b:c:')':' ':d:e:f:'-':g:h:i:j:""
