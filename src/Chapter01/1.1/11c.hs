import Data.Char

cipher_11c = "XJHRFTNZHMZGAHIUETXZJNBWNUTRHEPOMDNBJMAUGORFAOIZOCC"

cipher_12a = "LWKLQNWKDWLVKDOOQHYHUVHHDELOOERDUGORYHOBDVDWUHH"
cipher_12b = "UXENRBWXCUXENFQRLQJUCNABFQNWRCJUCNAJCRXWORWMB"
cipher_12c = "BGUTBMBGZTFHNLXMKTIPBMAVAXXLXTEPTRLEXTOXKHHFYHKMAXFHNLX"

shift :: Int -> Char -> Char
shift n = chr . sn . ord
  where m i = i + n
        modOnCrutches m n = if m `mod` n /= 0 then m `mod` n  else 26
        sn i
            | m i < 65 = 91 - ((65 - m i) `modOnCrutches` 26)
            | m i > 90 = 64 + (m i `mod` 90)
            | otherwise = m i


cenc = caesar
cdec = caesar . negate

caesar :: Int -> String -> String
caesar n = map (shift n)

d11c :: String
d11c = zipWith shift [-1, -2..] cipher_11c
