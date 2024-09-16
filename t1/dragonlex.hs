-- dragonlex.hs

-- im not going to lie, i used a lot of ChatGPT to convert from another language (either py/rust)
-- into the equivalent haskell, because sometimes the syntax in this language is real simple
-- the simplicity is nice but makes it damn hard to read unless you have an encoclypedic knowledge
-- of haskell. but the simplicity is also nice.

import System.Environment
import System.IO
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Control.Applicative
import System.Exit (exitFailure)

-- huh this syntax makes a lot more sense reading it in "verbose" rust
-- then coming back to haskell. wow. neat.
data Action = Skip
            | ErrorAction String
            | TokenAction String Bool

instance Show Action where
    show Skip = "Skip"
    show (ErrorAction msg) = "ErrorAction " ++ show msg
    show (TokenAction name keep) = "TokenAction " ++ show name ++ " " ++ show keep

data Rule = Rule String Action
    deriving Show

main :: IO ()
main = do
    args <- getArgs
    if length args < 1 then
        putStrLn "Usage: dragonlex <spec file>"
    else do
        let specFile = head args
        content <- readFile specFile
        let specLines = lines content
        case parseSpecLines specLines of
            Left errMsg -> do
                hPutStrLn stderr ("invalid spec: " ++ errMsg)
                exitFailure
            Right rules -> do
                template <- readFile "template.hs"
                -- this is where we replace
                let lexerCode = replacePlaceholder "{{RULES}}" (generateRulesCode rules) template
                writeFile "lexer.hs" lexerCode

-- Parsing the specification file
parseSpecLines :: [String] -> Either String [Rule]
parseSpecLines lines = parseLines lines 1 []
    where
        parseLines [] _ acc = Right (reverse acc)
        parseLines (line:rest) lineNum acc =
            case parseSpecLine line of
                Left err -> Left $ "Error on line " ++ show lineNum ++ ": " ++ err
                Right rule -> parseLines rest (lineNum + 1) (rule : acc)

parseSpecLine :: String -> Either String Rule
parseSpecLine line =
    let (regexStr, rest) = extractRegex line
        rest' = dropWhile isSpace rest
    in case readP_to_S actionParser rest' of
        [(action, "")] -> Right (Rule regexStr action)
        _ -> Left "Invalid action"

-- we need to extract REGEX which could contain whitespace as valid chars
-- and distinguish it from the whitespace seperator from ACTION
extractRegex :: String -> (String, String)
extractRegex line = extractRegex' line 0 0 ""
    where
        extractRegex' [] parenCount bracketCount acc =
            if parenCount == 0 && bracketCount == 0 then
                (acc, [])
            else
                ("", "") -- we have accumulated nothing (mismatched). RIP
        extractRegex' (c:cs) parenCount bracketCount acc =
            let (newParenCount, newBracketCount) = case c of
                    '(' -> (parenCount + 1, bracketCount)
                    ')' -> (parenCount - 1, bracketCount)
                    '[' -> (parenCount, bracketCount + 1)
                    ']' -> (parenCount, bracketCount - 1)
                    _   -> (parenCount, bracketCount)
                atTopLevel = (newParenCount == 0 && newBracketCount == 0)
            in if atTopLevel && isSpace c then
                 (acc, c:cs)
               else
                 extractRegex' cs newParenCount newBracketCount (acc ++ [c])

actionParser :: ReadP Action
actionParser = skipSpaces >> (
    (string "(SKIP)" >> return Skip)
    <|> (do
        string "(ERR)"
        skipSpaces
        msg <- quotedStringParser
        return (ErrorAction msg)
    )
    <|> (do
        token <- munch1 (\c -> isAlphaNum c || c == '_')
        skipSpaces
        keepStr <- string "true" <|> string "false"
        let keep = keepStr == "true"
        return (TokenAction token keep)
    )
  )

quotedStringParser :: ReadP String
quotedStringParser = do
    char '"'
    str <- manyTill (satisfy (\c -> c /= '"')) (char '"')
    return str

generateRulesCode :: [Rule] -> String
-- TIL: intercalate - to insert or position between or among existing elements or layers
-- https://hoogle.haskell.org/?hoogle=intercalate
generateRulesCode rules = trimWith (== ',') (intercalate "\n" (map showRule rules))

showRule :: Rule -> String
showRule (Rule regex action) =
    "        Rule " ++ show (trim regex) ++ "(" ++ show action ++ "),"

trim :: String -> String
trim = trimWith isSpace

trimWith :: (Char -> Bool) -> String -> String
trimWith p = f . f
  where f = reverse . dropWhile p

replacePlaceholder :: String -> String -> String -> String
replacePlaceholder placeholder replacement =
    intercalate replacement . splitOn placeholder

splitOn :: String -> String -> [String]
splitOn delim str = case breakOn delim str of
    (before, "") -> [before]
    (before, after) -> before : splitOn delim (drop (length delim) after)

breakOn :: String -> String -> (String, String)
breakOn delim str = case findIndex (isPrefixOf delim) (tails str) of
    Just index -> splitAt index str
    Nothing -> (str, "")
