import System.Environment
import Data.Char

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ convertSpaceMatch $ head args

convertSpaceMatch :: String -> String
convertSpaceMatch ""  = ""
convertSpaceMatch s   = convertRegexSpace $ convertRegexEscape $ toShortSpace s

-- 連続する空白文字を半角スペースに置き換える
toShortSpace :: String -> String
toShortSpace xs = toShortSpace' (map (\x -> if isSpace x then ' ' else x) xs) False

toShortSpace' :: String -> Bool -> String
toShortSpace' "" _            = ""
toShortSpace' (' ':xs) True   = toShortSpace' xs True
toShortSpace' (' ':xs) False  = ' ':toShortSpace' xs True
toShortSpace' (x:xs) _        = x:toShortSpace' xs False

-- 文字間の空白は一つ以上の空白とし、記号に隣接する空白は０個以上の空白とする
convertRegexSpace :: String -> String
convertRegexSpace ""          = ""
convertRegexSpace (' ':x:xs)  = if isSignChar x then "\\s*" ++ x:convertRegexSpace xs else "\\s+" ++ x:convertRegexSpace xs
convertRegexSpace (x:xs)      = x:convertRegexSpace xs

isSignChar :: Char -> Bool
isSignChar c = any (\a -> a == c) "\\!\"#$%&'()+-*/=^~|`@{}[]:;<>,.?_"

-- 正規表現の定義にならないように文字をエスケープする
convertRegexEscape :: String -> String
convertRegexEscape "" = ""
convertRegexEscape (x:xs) = if isRegexSpecialChar x then '\\':x:convertRegexEscape xs else x:convertRegexEscape xs

isRegexSpecialChar :: Char -> Bool
isRegexSpecialChar c = any (\a -> a == c) "\\$%&()+*/^[].?"
