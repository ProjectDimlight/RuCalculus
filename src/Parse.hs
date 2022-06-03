module Parse where
import AST

compareFirst :: String -> String -> Bool
compareFirst a b = ((head a) == (head b))

findQuote :: String -> (String, String)
findQuote s
    | compareFirst s "】" = ("】", tail s)
    | otherwise = ([head s] ++ fst quote, snd quote)
    where quote = findQuote (tail s)

tokenize :: String -> [String]
tokenize "" = []
tokenize s
    | compareFirst s "【" = [(fst quote)] ++ tokenize (snd quote)
    | otherwise = [[head s]] ++ tokenize (tail s)
    where quote = findQuote s
