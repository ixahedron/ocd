import Data.Char

e13a = "SCJAXUFBQKTPRWEZHVLIGYDNMO"
d13b = "dhbwoguqtcjsyxzlimakfrnevp"

plain = "The gold is hidden in the garden"
subst = "IBXLX JVXIZ SLLDE VAQLL DEVAU QLB"

encc :: String -> Char -> Char
encc _ ' ' = ' '
encc _ '.' = '.'
encc table a = table !! i
  where i = if ord a >= 97 then ord a - 97 else ord a - 65

encode :: String -> String
encode = map (encc e13a)

decode :: String -> String
decode = map (encc d13b)

