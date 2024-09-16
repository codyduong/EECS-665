-- template.hs

-- this is the intermediatery template, we replace a custom string with our rules
-- note that i don't say it because then it would be replaced... just read dragonlex.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import System.Environment
import System.IO
import Data.Char
import Data.List
import Text.Regex.TDFA
import Data.Maybe

-- multiline true is a terrible default and this caused so many headaches
(=~^) :: RegexContext Regex source target => source -> String -> target
(=~^) text pattern = match (makeRegexOpts defaultCompOpt{multiline=False} defaultExecOpt pattern :: Regex) text

type Position = (Int, Int)

main :: IO ()
main = do
    args <- getArgs
    if length args < 1 then
        putStrLn "Usage: lexer <input file>"
    else do
        let inputFile = head args
        content <- readFile inputFile
        tokenize content (1,1)

data Action = Skip
            | ErrorAction String
            | TokenAction String Bool
    deriving Show

data Rule = Rule String Action

rules :: [Rule]
rules = [
{{RULES}}
    ]

tokenize :: String -> Position -> IO ()
tokenize [] _ = return ()
tokenize input pos@(line, col) = do
    -- putStrLn $ "tokenize called with position [" ++ show line ++ "," ++ show col ++ "] and input: " ++ take 20 input ++ if length input > 20 then "..." else ""

    let (trimmedInput, newlinePos) = 
            let (newlines, rest) = span (== '\n') input
                newlineCount = length newlines
            in if newlineCount > 0
                then (rest, (line + newlineCount, 1))
                else (input, pos)

    let (input, pos) = (trimmedInput, newlinePos)
    let (line, col) = pos

    let indexedRules = zip [0..] rules
    case longestMatch input of
        Nothing -> putStrLn $ "Error: no matching rule at " ++ show pos
        Just (action, matched, rest) -> do
            case action of
                Skip -> tokenize rest (updatePos pos matched)
                ErrorAction msg -> putStrLn $ "Error at " ++ show pos ++ ": " ++ msg
                TokenAction tokenName displayValue -> do
                    let output = if displayValue
                                 then tokenName ++ ":" ++ matched ++ " [" ++ show line ++ "," ++ show col ++ "]"
                                 else tokenName ++ " [" ++ show line ++ "," ++ show col ++ "]"
                    putStrLn output
                    tokenize rest (updatePos pos matched)

longestMatch :: String -> Maybe (Action, String, String)
longestMatch input =
    let matches = mapMaybe (applyRule input) (zip [0..] rules)
    in if null matches then Nothing
       else
          let (_, action, lexeme, rest) = maximumBy compareMatches matches
          in Just (action, lexeme, rest)

compareMatches :: (Int, Action, String, String) -> (Int, Action, String, String) -> Ordering
compareMatches (idx1, _, lex1, _) (idx2, _, lex2, _) =
    case compare (length lex2) (length lex1) of
            LT -> GT
            GT -> LT
            EQ -> case compare idx1 idx2 of
                    LT -> GT -- we want the lowest indx not highest, so invert these
                    GT -> LT
                    EQ -> EQ

applyRule :: String -> (Int, Rule) -> Maybe (Int, Action, String, String)
applyRule input (idx, Rule regex action) =
    let (before, matched, rest) = input =~^ ("^(" ++ regex ++ ")") :: (String, String, String)
    in if not (null matched) then
           Just (idx, action, matched, rest)
       else
           Nothing

updatePos :: Position -> String -> Position
updatePos = foldl updateChar
  where
    updateChar (line, col) '\n' = (line + 1, 1)
    updateChar (line, col) _     = (line, col + 1)
