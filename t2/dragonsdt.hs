import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import System.Environment
import System.Exit (exitFailure, exitSuccess)
import System.IO
import System.Process (system)

data Symbol = NonTerminal String | Terminal String | ActionTrigger Int | Epsilon deriving (Show, Eq, Ord)

data ProductionRule = ProductionRule
  { lhs :: String,
    rhs :: [Symbol],
    index :: Int
  }
  deriving (Show)

type ActionCode = Map Int String

-- theres a lot of assumptions about the input.ag being simply correct
-- and not a lot of robust error handling (except LL1 errors, since that was a requirement)
-- it'll just crash without a meaningful description to end users... well good thing its just a
-- school project...

-- also at some point i should've learned like a haskell doc notation, or made up my own...
-- type signatures are simply not enough for me to remember what each parameter does... the names are pretty self explanatory,
-- but i always misorder the parameters.

main :: IO ()
main = do
  args <- getArgs
  let inputFile = if null args then "input.ag" else head args
  content <- readFile inputFile
  let linesOfFile = lines content
  let (grammarLines, initLines, actionLines) = splitSections linesOfFile

  let productionRules = parseGrammar grammarLines
  let nts = getNonTerminals productionRules
  let terms = getTerminals productionRules nts
  let actionCode = parseActions actionLines
  let initCode = unlines initLines

  let firstSets = computeFirstSets productionRules nts terms
  let followSets = computeFollowSets productionRules nts terms firstSets

  let parsingTable = buildParsingTable productionRules nts terms firstSets followSets
  -- Check for LL(1) conflicts
  if hasConflicts parsingTable
    then do
      putStrLn "Bad grammar"
      exitFailure
    else do
      templateContent <- readFile "template.cpp"
      let cppCode = generateCppCode templateContent initCode actionCode productionRules nts terms parsingTable
      writeFile "translator.cpp" cppCode
      exitSuccess

-- split into the appropriate sections broken by %%
splitSections :: [String] -> ([String], [String], [String])
splitSections lines =
  let (grammarLines, rest1) = break (== "%%") lines
      rest2 = drop 1 rest1
      (initLines, rest3) = break (== "%%") rest2
      rest4 = drop 1 rest3
      actionLines = rest4
   in (grammarLines, initLines, actionLines)

parseGrammar :: [String] -> [ProductionRule]
parseGrammar = zipWith parseProduction [1 ..]

parseProduction :: Int -> String -> ProductionRule
parseProduction idx line =
  case splitOn "::=" line of
    [lhsPart, rhsPart] ->
      let lhsSymbol = trim lhsPart
          rhsSymbols = parseRHS rhsPart
       in ProductionRule
            { lhs = lhsSymbol,
              rhs = rhsSymbols,
              index = idx
            }
    _ -> error ("Invalid production: " ++ line)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

parseRHS :: String -> [Symbol]
parseRHS rhsString =
  let tokens = words rhsString
   in if null tokens
        then [Epsilon]
        else map parseSymbol tokens

parseSymbol :: String -> Symbol
parseSymbol ('#' : rest) | all isDigit rest = ActionTrigger (read rest)
parseSymbol s
  | all isAlphaNum s =
      if isUpper (head s)
        then NonTerminal s
        else Terminal s
  | otherwise = error ("Invalid symbol: " ++ s)

getNonTerminals :: [ProductionRule] -> [String]
getNonTerminals prs = nub [lhs pr | pr <- prs]

getTerminals :: [ProductionRule] -> [String] -> [String]
getTerminals prs nts =
  nub
    [ s | pr <- prs, sym <- rhs pr, s <- case sym of
                                      Terminal t -> [t]
                                      _ -> []
    ]

parseActions :: [String] -> ActionCode
parseActions lines = Map.fromList (map parseAction (filter (not . null) lines))

parseAction :: String -> (Int, String)
parseAction line =
  let line' = dropWhile isSpace line
   in if null line'
        then error "Empty action line"
        else
          let ('#' : rest) = line'
              (numStr, rest1) = span isDigit rest
              num = read numStr
              codeWithBraces = dropWhile isSpace rest1
              code = extractCode codeWithBraces
           in (num, code)

extractCode :: String -> String
extractCode s =
  let s1 = dropWhile (/= '{') s
      s2 = drop 1 s1 -- consume '{'
      s3 = takeWhile (/= '}') s2
   in s3

computeFirstSets :: [ProductionRule] -> [String] -> [String] -> Map String [String]
computeFirstSets prs nts terms = fixPoint initialFirstSets
  where
    initialFirstSets = Map.fromList [(nt, []) | nt <- nts]
    fixPoint firstSets =
      let updatedFirstSets = foldl' (computeFirstSet prs) firstSets nts
       in if updatedFirstSets == firstSets then firstSets else fixPoint updatedFirstSets

computeFirstSet :: [ProductionRule] -> Map String [String] -> String -> Map String [String]
computeFirstSet prs firstSets nt =
  let productions = [pr | pr <- prs, lhs pr == nt]
      firstNt = concatMap (firstOfProduction firstSets) productions
      existingFirst = Map.findWithDefault [] nt firstSets
      updatedFirst = nub (existingFirst ++ firstNt)
   in Map.insert nt updatedFirst firstSets

firstOfProduction :: Map String [String] -> ProductionRule -> [String]
firstOfProduction firstSets pr = firstOfSymbols firstSets (rhs pr)

firstOfSymbols :: Map String [String] -> [Symbol] -> [String]
firstOfSymbols _ [] = ["$"]
firstOfSymbols firstSets (sym : rest) =
  case sym of
    Terminal t -> [t]
    NonTerminal nt ->
      let firstNt = Map.findWithDefault [] nt firstSets
       in if "$" `elem` firstNt
            then nub (delete "$" firstNt ++ firstOfSymbols firstSets rest)
            else firstNt
    ActionTrigger _ -> firstOfSymbols firstSets rest
    Epsilon -> ["$"]

computeFollowSets :: [ProductionRule] -> [String] -> [String] -> Map String [String] -> Map String [String]
computeFollowSets prs nts terms firstSets = fixPoint initialFollowSets
  where
    initialFollowSets = Map.fromList [(nt, ["$" | nt == startSymbol]) | nt <- nts]
    startSymbol = lhs (head prs)
    fixPoint followSets =
      let updatedFollowSets = foldl' (computeFollowSet prs firstSets) followSets prs
       in if updatedFollowSets == followSets then followSets else fixPoint updatedFollowSets

computeFollowSet :: [ProductionRule] -> Map String [String] -> Map String [String] -> ProductionRule -> Map String [String]
computeFollowSet prs firstSets followSets pr =
  foldl' (updateFollowSet firstSets (lhs pr)) followSets (zip (rhs pr) (tail (tails (rhs pr))))

updateFollowSet :: Map String [String] -> String -> Map String [String] -> (Symbol, [Symbol]) -> Map String [String]
updateFollowSet firstSets lhsNt followSets (sym, beta) =
  case sym of
    NonTerminal nt ->
      let firstBeta = firstOfSymbols firstSets beta
          firstBetaNoEps = delete "$" firstBeta
          followNt = Map.findWithDefault [] nt followSets
          followLhs = Map.findWithDefault [] lhsNt followSets
          updatedFollowNt = nub (followNt ++ firstBetaNoEps ++ if "$" `elem` firstBeta then followLhs else [])
       in Map.insert nt updatedFollowNt followSets
    _ -> followSets

buildParsingTable :: [ProductionRule] -> [String] -> [String] -> Map String [String] -> Map String [String] -> Map (String, String) [ProductionRule]
buildParsingTable prs nts terms firstSets followSets = Map.fromListWith (++) tableEntries
  where
    tableEntries = concatMap buildEntries prs
    buildEntries pr =
      let a = lhs pr
          alpha = rhs pr
          firstAlpha = firstOfSymbols firstSets alpha
          entries =
            [((a, t), [pr]) | t <- firstAlpha, t /= "$"]
              ++ if "$" `elem` firstAlpha
                then
                  [((a, t), [pr]) | t <- Map.findWithDefault [] a followSets]
                else []
       in entries

hasConflicts :: Map (String, String) [ProductionRule] -> Bool
hasConflicts parsingTable = any (\prs -> length prs > 1) (Map.elems parsingTable)

generateCppCode :: String -> String -> ActionCode -> [ProductionRule] -> [String] -> [String] -> Map (String, String) [ProductionRule] -> String
generateCppCode template initCode actionCode prs nts terms parsingTable =
  let code = template
      -- lol
      code1 = replace "//{{INIT_CODE}}" initCode code
      code2 = replace "//{{TERMINALS_SET}}" (generateSet terms) code1
      code3 = replace "//{{NONTERMINALS_SET}}" (generateSet nts) code2
      code4 = replace "{{START_SYMBOL}}" (lhs (head prs)) code3
      code5 = replace "//{{ACTION_CODE}}" (generateActionExecutionCode actionCode) code4
      code6 = replace "//{{PARSING_TABLE_ENTRIES}}" (generateParsingTableCode parsingTable) code5
   in code6

replace :: String -> String -> String -> String
replace placeholder value = intercalate value . splitOn placeholder

splitOn :: String -> String -> [String]
splitOn delimiter = go
  where
    go str =
      case breakOn delimiter str of
        (before, "") -> [before]
        (before, after) -> before : go (drop (length delimiter) after)

breakOn :: String -> String -> (String, String)
breakOn delimiter str =
  case findIndex (isPrefixOf delimiter) (tails str) of
    Just idx -> splitAt idx str
    Nothing -> (str, "")

generateSet :: [String] -> String
generateSet symbols = "{" ++ intercalate ", " (map showStringLit symbols) ++ "}"

showStringLit :: String -> String
showStringLit s = "\"" ++ s ++ "\""

generateActionExecutionCode :: ActionCode -> String
generateActionExecutionCode actionCode =
  let codeLines = ["if (actionNum == " ++ show num ++ ") {\n               " ++ code ++ "\n            }" | (num, code) <- Map.toList actionCode]
   in intercalate " else " codeLines ++ ";"

generateParsingTableCode :: Map (String, String) [ProductionRule] -> String
generateParsingTableCode parsingTable =
  -- chatgpt thank you. haskell not having multiline strings is horrible...
  -- also im thankful this time i used cpp as an intermediary in some aspects, since it is whitespace
  -- agnostic. caused a lot of trouble trying to remember how many spaces i needed in dragonlex
  let entries =
        [ "    { std::make_pair("
            ++ showStringLit nt
            ++ ", "
            ++ showStringLit t
            ++ "), ProductionRule{ "
            ++ showStringLit prlhs
            ++ ", {"
            ++ intercalate ", " (map showSymbol (rhs pr))
            ++ "} } }"
          | ((nt, t), [pr]) <- Map.toList parsingTable,
            let prlhs = lhs pr
        ]
   in intercalate ",\n" entries

showSymbol :: Symbol -> String
showSymbol (Terminal s) = "Symbol{" ++ showStringLit "Terminal" ++ ", " ++ showStringLit s ++ "}"
showSymbol (NonTerminal s) = "Symbol{" ++ showStringLit "NonTerminal" ++ ", " ++ showStringLit s ++ "}"
showSymbol (ActionTrigger n) = "Symbol{" ++ showStringLit "ActionTrigger" ++ ", " ++ showStringLit ("#" ++ show n) ++ "}"
showSymbol Epsilon = "Symbol{" ++ showStringLit "$" ++ ", " ++ showStringLit "$" ++ "}"
