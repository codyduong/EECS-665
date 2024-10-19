{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import System.Environment
import System.Exit (exitFailure, exitSuccess)
import System.IO
import System.Process (system)

data Symbol = Symbol String | ActionTrigger Int | Epsilon deriving (Show, Eq, Ord)

data ProductionRule = ProductionRule
  { lhs :: String,
    rhs :: [Symbol],
    index :: Int
  }
  deriving (Show, Eq, Ord)

data LR0Item = LR0Item
  { itemPr :: ProductionRule,
    itemDotPos :: Int
  }
  deriving (Eq, Ord, Show)

data Action = Shift Int | Reduce ProductionRule | Accept deriving (Show)

type State = Set.Set LR0Item

type ActionTable = Map (Int, String) Action

type GotoTable = Map (Int, String) Int

type ActionCode = Map Int String

main :: IO ()
main = do
  args <- getArgs
  let inputFile = if null args then "input.ag" else head args
  content <- readFile inputFile
  let linesOfFile = lines content
  let (grammarLines, initLines, actionLines) = splitSections linesOfFile

  let productionRules = augmentGrammar $ parseGrammar grammarLines
  let nonTerminals = getNonTerminals productionRules
  let terminals = getTerminals productionRules nonTerminals
  let actionCode = parseActions actionLines
  let initCode = unlines initLines

  let firstSets = computeFirstSets productionRules nonTerminals
  let followSets = computeFollowSets productionRules nonTerminals firstSets

  let (states, transitions) = buildCanonicalCollection productionRules nonTerminals
  let (actionTable, gotoTable) = buildActionGotoTables productionRules states transitions terminals followSets nonTerminals

  -- Check for conflicts
  if hasConflicts actionTable
    then do
      putStrLn "Bad grammar"
      exitFailure
    else do
      templateContent <- readFile "template.cpp"
      let cppCode = generateCppCode templateContent initCode actionCode productionRules nonTerminals terminals actionTable gotoTable
      writeFile "parser.cpp" cppCode
      generateDotFile states transitions
      exitSuccess

splitSections :: [String] -> ([String], [String], [String])
splitSections lines =
  let (grammarLines, rest1) = break (== "%%") lines
      rest2 = drop 1 rest1
      (initLines, rest3) = break (== "%%") rest2
      rest4 = drop 1 rest3
      actionLines = rest4
   in (grammarLines, initLines, actionLines)

parseGrammar :: [String] -> [ProductionRule]
parseGrammar lines = snd $ foldl' processLine (Nothing, []) (zip [1 ..] lines)
  where
    processLine :: (Maybe String, [ProductionRule]) -> (Int, String) -> (Maybe String, [ProductionRule])
    processLine (currentLHS, prs) (idx, line) =
      let line' = trim line
       in if null line'
            then (currentLHS, prs)
            else
              if "::=" `isInfixOf` line'
                then case splitOn "::=" line' of
                  [lhsPart, rhsPart] ->
                    let lhsSymbol = trim lhsPart
                        rhsSymbols = parseRHS rhsPart
                        pr = ProductionRule {lhs = lhsSymbol, rhs = rhsSymbols, index = idx}
                     in (Just lhsSymbol, prs ++ [pr])
                  _ -> error ("Invalid production at line " ++ show idx ++ ": " ++ line')
                else
                  if not (null line') && (head line' == '|')
                    then case currentLHS of
                      Just lhsSymbol ->
                        let rhsPart = drop 1 line' -- drop the '|'
                            rhsSymbols = parseRHS rhsPart
                            pr = ProductionRule {lhs = lhsSymbol, rhs = rhsSymbols, index = idx}
                         in (currentLHS, prs ++ [pr])
                      Nothing -> error ("'|' encountered before any LHS symbol at line: " ++ show idx)
                    else error ("Invalid line at line: " ++ show idx ++ " - " ++ line')

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
  | all isAlphaNum s = Symbol s
  | otherwise = error ("Invalid symbol: " ++ s)

getNonTerminals :: [ProductionRule] -> Set.Set String
getNonTerminals prs = Set.fromList [lhs pr | pr <- prs]

getTerminals :: [ProductionRule] -> Set.Set String -> Set.Set String
getTerminals prs nonTerminals =
  Set.fromList
    [ s | pr <- prs, sym <- rhs pr, s <- case sym of
                                      Symbol t -> [t]
                                      _ -> [], not (Set.member s nonTerminals)
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

computeFirstSets :: [ProductionRule] -> Set.Set String -> Map String [String]
computeFirstSets prs nonTerminals = fixPoint initialFirstSets
  where
    initialFirstSets = Map.fromList [(nt, []) | nt <- Set.toList nonTerminals]
    fixPoint firstSets =
      let updatedFirstSets = foldl' (computeFirstSet prs nonTerminals) firstSets (Set.toList nonTerminals)
       in if updatedFirstSets == firstSets then firstSets else fixPoint updatedFirstSets

computeFirstSet :: [ProductionRule] -> Set.Set String -> Map String [String] -> String -> Map String [String]
computeFirstSet prs nonTerminals firstSets nt =
  let productions = [pr | pr <- prs, lhs pr == nt]
      firstNt = concatMap (firstOfProduction firstSets nonTerminals) productions
      existingFirst = Map.findWithDefault [] nt firstSets
      updatedFirst = nub (existingFirst ++ firstNt)
   in Map.insert nt updatedFirst firstSets

firstOfProduction :: Map String [String] -> Set.Set String -> ProductionRule -> [String]
firstOfProduction firstSets nonTerminals pr = firstOfSymbols firstSets nonTerminals (rhs pr)

firstOfSymbols :: Map String [String] -> Set.Set String -> [Symbol] -> [String]
firstOfSymbols _ _ [] = ["$"]
firstOfSymbols firstSets nonTerminals (sym : rest) =
  case sym of
    Symbol s ->
      if Set.member s nonTerminals
        then
          let firstNt = Map.findWithDefault [] s firstSets
           in if "$" `elem` firstNt
                then nub (delete "$" firstNt ++ firstOfSymbols firstSets nonTerminals rest)
                else firstNt
        else [s] -- It's a terminal
    ActionTrigger _ -> firstOfSymbols firstSets nonTerminals rest
    Epsilon -> ["$"]

computeFollowSets :: [ProductionRule] -> Set.Set String -> Map String [String] -> Map String [String]
computeFollowSets prs nonTerminals firstSets = fixPoint initialFollowSets
  where
    initialFollowSets = Map.fromList [(nt, ["$" | nt == startSymbol]) | nt <- Set.toList nonTerminals]
    startSymbol = lhs (head prs)
    fixPoint followSets =
      let updatedFollowSets = foldl' (computeFollowSet prs nonTerminals firstSets) followSets prs
       in if updatedFollowSets == followSets then followSets else fixPoint updatedFollowSets

computeFollowSet :: [ProductionRule] -> Set.Set String -> Map String [String] -> Map String [String] -> ProductionRule -> Map String [String]
computeFollowSet prs nonTerminals firstSets followSets pr =
  foldl' (updateFollowSet firstSets nonTerminals (lhs pr)) followSets (zip (rhs pr) (tail (tails (rhs pr))))

updateFollowSet :: Map String [String] -> Set.Set String -> String -> Map String [String] -> (Symbol, [Symbol]) -> Map String [String]
updateFollowSet firstSets nonTerminals lhsNt followSets (sym, beta) =
  case sym of
    Symbol s ->
      if Set.member s nonTerminals
        then
          let firstBeta = firstOfSymbols firstSets nonTerminals beta
              firstBetaNoEps = delete "$" firstBeta
              followNt = Map.findWithDefault [] s followSets
              followLhs = Map.findWithDefault [] lhsNt followSets
              updatedFollowNt = nub (followNt ++ firstBetaNoEps ++ if "$" `elem` firstBeta then followLhs else [])
           in Map.insert s updatedFollowNt followSets
        else followSets
    _ -> followSets

augmentGrammar :: [ProductionRule] -> [ProductionRule]
augmentGrammar prs =
  let oldStartSymbol = lhs (head prs)
      newStartSymbol = oldStartSymbol ++ "'"
      newProduction = ProductionRule {lhs = newStartSymbol, rhs = [Symbol oldStartSymbol], index = 0}
   in newProduction : prs

symbolAfterDot :: LR0Item -> Maybe Symbol
symbolAfterDot item =
  if itemDotPos item < length (rhs (itemPr item))
    then Just (rhs (itemPr item) !! itemDotPos item)
    else Nothing

closure :: Set.Set LR0Item -> [ProductionRule] -> Set.Set String -> Set.Set LR0Item
closure items prs nonTerminals = fixPoint items
  where
    fixPoint currentItems =
      let newItems = Set.foldl' (addItems nonTerminals) currentItems currentItems
       in if newItems == currentItems then currentItems else fixPoint newItems

    addItems nonTerminals acc item =
      case symbolAfterDot item of
        Just (Symbol s)
          | Set.member s nonTerminals ->
              let newItems = [LR0Item pr 0 | pr <- prs, lhs pr == s]
               in Set.union acc (Set.fromList newItems)
        Just Epsilon ->
          let movedItem = item {itemDotPos = itemDotPos item + 1}
           in Set.insert movedItem acc
        _ -> acc

buildCanonicalCollection :: [ProductionRule] -> Set.Set String -> (Map Int State, Map Int (Map Symbol Int))
buildCanonicalCollection prs nonTerminals =
  let initialItem = LR0Item (head prs) 0
      initialState = closure (Set.singleton initialItem) prs nonTerminals
      initialStateNum = 0
      stateMap = Map.singleton initialState initialStateNum
      reverseStateMap = Map.singleton initialStateNum initialState
      transitions = Map.empty :: Map Int (Map Symbol Int)
      go :: Map State Int -> Map Int State -> Map Int (Map Symbol Int) -> [State] -> (Map Int State, Map Int (Map Symbol Int))
      go stateMap reverseStateMap transitions [] = (reverseStateMap, transitions)
      go stateMap reverseStateMap transitions (state : restStates) =
        let stateNum = stateMap Map.! state
            symbols = nub $ catMaybes [symbolAfterDot item | item <- Set.toList state]
            symbols' = filter (\s -> case s of Epsilon -> False; _ -> True) symbols
            (newStateMap, newReverseStateMap, newTransitions, newStates) = foldl' (processSymbol state stateNum) (stateMap, reverseStateMap, transitions, []) symbols'
            allRestStates = restStates ++ newStates
         in go newStateMap newReverseStateMap newTransitions allRestStates

      processSymbol state stateNum (stateMap, reverseStateMap, transitions, newStates) sym =
        let toState = goto state sym prs nonTerminals
         in case Map.lookup toState stateMap of
              Just toStateNum ->
                let updatedTransitions = Map.insertWith Map.union stateNum (Map.singleton sym toStateNum) transitions
                 in (stateMap, reverseStateMap, updatedTransitions, newStates)
              Nothing ->
                let newStateNum = Map.size stateMap
                    newStateMap' = Map.insert toState newStateNum stateMap
                    newReverseStateMap' = Map.insert newStateNum toState reverseStateMap
                    updatedTransitions = Map.insertWith Map.union stateNum (Map.singleton sym newStateNum) transitions
                 in (newStateMap', newReverseStateMap', updatedTransitions, newStates ++ [toState])
   in go stateMap reverseStateMap transitions [initialState]
  where
    goto :: State -> Symbol -> [ProductionRule] -> Set.Set String -> State
    goto items sym prs nonTerminals =
      let movedItems =
            Set.fromList
              [ LR0Item (itemPr item) (itemDotPos item + 1)
                | item <- Set.toList items,
                  symbolAfterDot item == Just sym
              ]
       in closure movedItems prs nonTerminals

buildActionGotoTables :: [ProductionRule] -> Map Int State -> Map Int (Map Symbol Int) -> Set.Set String -> Map String [String] -> Set.Set String -> (ActionTable, GotoTable)
buildActionGotoTables prs stateMap transitions terminals followSets nonTerminals =
  let allStates = Map.keys stateMap
      actionTable = Map.empty :: ActionTable
      gotoTable = Map.empty :: GotoTable
      (actionTable', gotoTable') = foldl' processState (actionTable, gotoTable) allStates
   in (actionTable', gotoTable')
  where
    processState (actionAcc, gotoAcc) stateNum =
      let items = Map.findWithDefault Set.empty stateNum stateMap
          actionAcc' = Set.foldl' (processItem stateNum nonTerminals) actionAcc items
          gotoAcc' = case Map.lookup stateNum transitions of
            Just transMap ->
              Map.foldlWithKey' (processTransition stateNum) gotoAcc transMap
            Nothing -> gotoAcc
       in (actionAcc', gotoAcc')

    processItem stateNum nonTerminals actionAcc item =
      case symbolAfterDot item of
        Just (Symbol a) | not (Set.member a nonTerminals) ->
          case Map.lookup stateNum transitions >>= Map.lookup (Symbol a) of
            Just s' -> Map.insert (stateNum, a) (Shift s') actionAcc
            Nothing -> actionAcc
        Just (ActionTrigger _) -> actionAcc -- Ignore action triggers in ACTION table construction
        Just Epsilon ->
          -- Move the dot over Epsilon
          let movedItem = item {itemDotPos = itemDotPos item + 1}
           in processItem stateNum nonTerminals actionAcc movedItem
        Nothing ->
          let lhsSymbol = lhs (itemPr item)
              alpha = rhs (itemPr item)
           in if lhsSymbol == lhs (head prs) && itemDotPos item == length alpha
                then Map.insert (stateNum, "$") Accept actionAcc
                else
                  let follows = Map.findWithDefault [] lhsSymbol followSets
                      actionAcc' = foldl' (\acc a -> Map.insert (stateNum, a) (Reduce (itemPr item)) acc) actionAcc follows
                   in actionAcc'
        _ -> actionAcc

    processTransition stateNum gotoAcc sym s' =
      case sym of
        Symbol nt | Set.member nt nonTerminals -> Map.insert (stateNum, nt) s' gotoAcc
        _ -> gotoAcc

hasConflicts :: ActionTable -> Bool
hasConflicts actionTable = any (\actions -> length actions > 1) groupedActions
  where
    groupedActions = Map.elems $ Map.fromListWith (++) [((state, symbol), [action]) | ((state, symbol), action) <- Map.toList actionTable]

generateCppCode :: String -> String -> ActionCode -> [ProductionRule] -> Set.Set String -> Set.Set String -> ActionTable -> GotoTable -> String
generateCppCode template initCode actionCode prs nonTerminals terminals actionTable gotoTable =
  let code = template
      code1 = replace "//{{INIT_CODE}}" initCode code
      code2 = replace "//{{TERMINALS_SET}}" (generateSet (Set.toList terminals)) code1
      code3 = replace "//{{NONTERMINALS_SET}}" (generateSet (Set.toList nonTerminals)) code2
      code4 = replace "//{{ACTION_TABLE}}" (generateActionTableCode nonTerminals terminals actionTable) code3
      code5 = replace "//{{GOTO_TABLE}}" (generateGotoTableCode gotoTable) code4
      code6 = replace "//{{ACTION_CODE}}" (generateActionExecutionCode actionCode) code5
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

generateActionTableCode :: Set.Set String -> Set.Set String -> ActionTable -> String
generateActionTableCode nonTerminals terminals actionTable =
  let entries = [ "    { std::make_pair(" ++ show stateNum ++ ", " ++ showStringLit symbol ++ "), " ++ generateActionCode nonTerminals terminals action ++ " }"
                | ((stateNum, symbol), action) <- Map.toList actionTable
                ]
   in intercalate ",\n" entries

generateActionCode :: Set.Set String -> Set.Set String -> Action -> String
generateActionCode nonTerminals terminals (Shift s') = "Action{Shift, " ++ show s' ++ ", ProductionRule()}"
generateActionCode nonTerminals terminals (Reduce pr) = "Action{Reduce, 0, " ++ generateProductionRuleCode nonTerminals terminals pr ++ "}"
generateActionCode _ _ Accept = "Action{Accept, 0, ProductionRule()}"

generateProductionRuleCode :: Set.Set String -> Set.Set String -> ProductionRule -> String
generateProductionRuleCode nonTerminals terminals pr =
  "ProductionRule{ " ++ showStringLit (lhs pr) ++ ", {" ++ intercalate ", " (map (showSymbol nonTerminals terminals) (rhs pr)) ++ "} }"

showSymbol :: Set.Set String -> Set.Set String -> Symbol -> String
showSymbol nonTerminals terminals (Symbol s)
  | Set.member s nonTerminals = "Symbol{" ++ showStringLit "NonTerminal" ++ ", " ++ showStringLit s ++ ", 0}"
  | Set.member s terminals = "Symbol{" ++ showStringLit "Terminal" ++ ", " ++ showStringLit s ++ ", 0}"
  | otherwise = error ("Symbol " ++ s ++ " not found in nonTerminals or terminals")
showSymbol _ _ (ActionTrigger n) = "Symbol{" ++ showStringLit "ActionTrigger" ++ ", \"\", " ++ show n ++ "}"
showSymbol _ _ Epsilon = "Symbol{" ++ showStringLit "Epsilon" ++ ", " ++ showStringLit "$" ++ ", 0}"


generateGotoTableCode :: GotoTable -> String
generateGotoTableCode gotoTable =
  let entries =
        [ "    { std::make_pair(" ++ show stateNum ++ ", " ++ showStringLit symbol ++ "), " ++ show s' ++ " }"
          | ((stateNum, symbol), s') <- Map.toList gotoTable
        ]
   in intercalate ",\n" entries

generateDotFile :: Map Int State -> Map Int (Map Symbol Int) -> IO ()
generateDotFile states transitions = do
    let dotContent = generateDot states transitions
    withFile "language.dot" WriteMode $ \handle -> do
        hSetEncoding handle utf8
        hPutStr handle dotContent

generateDot :: Map Int State -> Map Int (Map Symbol Int) -> String
generateDot states transitions =
    let header = "digraph G {\n"
        nodeDefs = concatMap (generateNodeDef states) (Map.keys states)
        edgeDefs = concatMap (generateEdgeDefs transitions) (Map.keys transitions)
        footer = "}\n"
    in header ++ nodeDefs ++ edgeDefs ++ footer

generateNodeDef :: Map Int State -> Int -> String
generateNodeDef states stateNum =
    let items = Map.findWithDefault Set.empty stateNum states
        label = "State " ++ show stateNum ++ "\\n" ++ concatMap (escape . showItem) (Set.toList items)
    in "  " ++ show stateNum ++ " [label=\"" ++ label ++ "\", shape=box];\n"

showItem :: LR0Item -> String
showItem item =
    let pr = itemPr item
        symbols = rhs pr
        beforeDot = take (itemDotPos item) symbols
        afterDot = drop (itemDotPos item) symbols
        showSymbols syms = unwords (map showSymbolDot syms)
        symbolStr = showSymbols beforeDot ++ " • " ++ showSymbols afterDot
    in lhs pr ++ " -> " ++ symbolStr ++ "\\n"

generateEdgeDefs :: Map Int (Map Symbol Int) -> Int -> String
generateEdgeDefs transitions stateNum =
    case Map.lookup stateNum transitions of
        Just transMap -> concatMap (generateEdge stateNum) (Map.toList transMap)
        Nothing -> ""


showSymbolDot :: Symbol -> String
showSymbolDot (Symbol s) = s
showSymbolDot (ActionTrigger n) = "#" ++ show n
showSymbolDot Epsilon = "ε"

generateEdge :: Int -> (Symbol, Int) -> String
generateEdge fromState (symbol, toState) =
    let label = showSymbolDot symbol
    in "  " ++ show fromState ++ " -> " ++ show toState ++ " [label=\"" ++ escape label ++ "\"];\n"

escape :: String -> String
escape = concatMap escapeChar

escapeChar :: Char -> String
escapeChar '"' = "\\\""
escapeChar '\\' = "\\\\"
escapeChar '\n' = "\\n"
escapeChar c = [c]