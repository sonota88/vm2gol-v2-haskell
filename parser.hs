import Types
import qualified Json as Json

data TToken =
  Ident String Int
  | Kw String Int
  | Sym String Int
  | Int String Int
  | Str String Int
  deriving Show

main :: IO ()
main = do
  src <- getContents
  putStr ((Main.parse src) ++ "\n")

tokenVal :: TToken -> String
tokenVal t =
  case t of
    Kw    v _ -> v
    Ident v _ -> v
    Sym   v _ -> v
    Int   v _ -> v
    Str   v _ -> v

consume :: [TToken] -> String -> ([TToken])
consume ts expVal =
  let t : ts2 = ts
  in
    if (tokenVal t) == expVal then
      ts2
    else
      error ("unexpected token: exp (" ++ expVal ++ ") act (" ++ (show t) ++ ")")

takeVal :: [TToken] -> ([TToken], String)
takeVal ts =
  case ts of
    t : ts2 -> (ts2, (tokenVal t))
    _ -> error "must_not_happen"

-- --------------------------------

parseArgs :: [TToken] -> ([TToken], [TNode])
parseArgs ts =
  case ts of
    Sym ")" _ : _ ->
      (ts, [])
    Ident v _ : rest ->
      let (ts10, restArgs) = (parseArgs rest)
      in
        (
          ts10,
          StrNode v : restArgs
        )
    Int v _ : rest ->
      let (ts10, restArgs) = (parseArgs rest)
      in
        (
          ts10,
          IntNode (read v) : restArgs
        )
    Sym "," _ : rest -> parseArgs rest
    _ -> error "unexpected_pattern"

_parseExprRight :: [TToken] -> Maybe ([TToken], String, TNode)
_parseExprRight ts =
  case ts of
    Sym v _ : rest ->
      if (v == "+")
         || (v == "*")
         || (v == "==")
         || (v == "!=")
      then
        let (ts10, exprR) = parseExpr rest
        in
          Just (ts10, v, exprR)
      else
        Nothing
    _ -> error "must be symbol"

parseExpr :: [TToken] -> ([TToken], TNode)
parseExpr ts =
  let (ts10, exprL) =
        case ts of
          Int v _ : rest ->
            (rest, IntNode (read v))
          Ident v _ : rest ->
            (rest, StrNode v)
          Sym _ _ : _ ->
            let ts50 = consume ts "("
                (ts60, expr) = parseExpr ts50
                ts70 = consume ts60 ")"
            in
              (ts70, expr)
          _ -> error "unexpected_pattern"
  in
  let retval = _parseExprRight ts10 in
  case retval of
    Just (ts20, op, exprR) ->
      (
        ts20,
        ListNode [
          StrNode op,
          exprL,
          exprR
          ]
      )
    Nothing -> (ts10, exprL)

parseSet :: [TToken] -> ([TToken], [TNode])
parseSet ts =
  let ts10 = consume ts "set"
      (ts20, varName) = takeVal ts10
      ts30 = consume ts20 "="
      (ts50, expr) = parseExpr ts30
      ts60 = consume ts50 ";"
  in
    (
      ts60,
      [
        StrNode "set",
        StrNode varName,
        expr
      ]
    )

parseCall :: [TToken] -> ([TToken], [TNode])
parseCall ts =
  case ts of
    Kw "call" _ : Ident fnName _ : Sym "(" _ : ts10 ->
      let (ts20, args) = parseArgs ts10
          ts30 = consume ts20 ")"
          ts40 = consume ts30 ";"
      in
        (ts40,
          StrNode "call"
          : StrNode fnName
          : args
        )
    _ -> error "unexpected_pattern"

parseCallSet :: [TToken] -> ([TToken], [TNode])
parseCallSet ts =
  case ts of
    Kw "call_set" _ : Ident varName _ : Sym "=" _ : Ident fnName _ : ts10 ->
      let ts20 = consume ts10 "("
          (ts30, args) = parseArgs ts20
          ts35 = consume ts30 ")"
          ts40 = consume ts35 ";"
      in
        (
          ts40,
          StrNode "call_set"
          : StrNode varName
          : [ListNode (StrNode fnName : args)]
        )
    _ -> error "unexpected_pattern"

parseReturn :: [TToken] -> ([TToken], [TNode])
parseReturn ts =
  case ts of
    Kw "return" _ : ts20 ->
      let (ts30, expr) = parseExpr ts20
          ts40 = consume ts30 ";"
      in
        (
          ts40,
          [StrNode "return", expr]
        )
    _ -> error "unexpected_pattern"

parseWhile :: [TToken] -> ([TToken], [TNode])
parseWhile ts =
  let
    ts20 = consume ts "while"
    ts30 = consume ts20 "("
    (ts40, expr) = parseExpr ts30
    ts50 = consume ts40 ")"
    ts60 = consume ts50 "{"
    (ts70, stmts) = parseStmts ts60
    ts80 = consume ts70 "}"
  in
    (
      ts80,
      [
        StrNode "while",
        expr,
        ListNode stmts
      ]
    )

_parseWhenClauses :: [TToken] -> ([TToken], [TNode])
_parseWhenClauses ts =
  case ts of
    Sym "}" _ : _ ->
      (ts, [])
    _ ->
      let
        ts20 = consume ts "("
        (ts30, expr) = parseExpr ts20
        ts40 = consume ts30 ")"
        ts60 = consume ts40 "{"
        (ts70, stmts) = parseStmts ts60
        ts80 = consume ts70 "}"
        (ts90, wcRest) = _parseWhenClauses ts80
      in
        (
          ts90,
          ListNode (expr : stmts) : wcRest
        )

parseCase :: [TToken] -> ([TToken], [TNode])
parseCase ts =
  let
    ts20 = consume ts "case"
    ts60 = consume ts20 "{"
    (ts70, whenClauses) = _parseWhenClauses ts60
    ts80 = consume ts70 "}"
  in
    (
      ts80,
      StrNode "case" : whenClauses
    )
  
parseVmComment :: [TToken] -> ([TToken], [TNode])
parseVmComment ts =
  let
    ts10 = consume ts "_cmt"
    ts20 = consume ts10 "("
    (ts30, cmt) = takeVal ts20
    ts40 = consume ts30 ")"
    ts50 = consume ts40 ";"
  in
    (
      ts50,
      [
        StrNode "_cmt",
        StrNode cmt
      ]
    )

parseStmts :: [TToken] -> ([TToken], [TNode])
parseStmts ts =
  case ts of
    Sym "}" _ : _ -> (ts, [])
    _ ->
      let (ts20, stmt) = parseStmt ts
          (ts30, rest) = parseStmts ts20
      in
        (
          ts30,
          (ListNode stmt) : rest
        )

parseStmt :: [TToken] -> ([TToken], [TNode])
parseStmt ts =
  case ts of
    (t : _) ->
      case (tokenVal t) of
        "set"      -> parseSet ts
        "_cmt"     -> parseVmComment ts
        "call"     -> parseCall ts
        "return"   -> parseReturn ts
        "call_set" -> parseCallSet ts
        "while"    -> parseWhile ts
        "case"     -> parseCase ts
        _ -> error "unexpected token"
    _ -> error "invalid statement"

parseVar :: [TToken] -> ([TToken], [TNode])
parseVar ts =
  let ts10 = consume ts "var"
      (ts30, varName) = takeVal ts10
  in
    if (tokenVal (head ts30)) == "=" then
      let ts40 = consume ts30 "="
          (ts50, expr) = parseExpr ts40
          ts60 = consume ts50 ";"
      in
        (
          ts60,
          [
            StrNode "var",
            StrNode varName,
            expr
          ]
        )
    else
      let ts40 = consume ts30 ";"
      in
        (
          ts40,
          [
            StrNode "var",
            StrNode varName
          ]
        )

parseFuncBody :: [TToken] -> ([TToken], [TNode])
parseFuncBody ts =
  case ts of
    [] -> error "must_not_happen"
    t : _ ->
      case (tokenVal t) of
        "}" -> (ts, [])
        "var" ->
          let (ts30, stmt) = (parseVar ts)
              (ts40, restStmts) = (parseFuncBody ts30)
          in
            (ts40, (ListNode stmt) : restStmts)
        _ ->
          let (ts30, stmt) = (parseStmt ts)
              (ts40, restStmts) = (parseFuncBody ts30)
          in
            (ts40, (ListNode stmt) : restStmts)

parseFuncDef :: [TToken] -> ([TToken], [TNode])
parseFuncDef ts =
  let ts10 = consume ts "func"
      (ts30, fnName) = takeVal ts10
      ts40 = consume ts30 "("
      (ts45, args) = parseArgs ts40
      ts50 = consume ts45 ")"
      ts60 = consume ts50 "{"
      (ts65, body) = parseFuncBody ts60
      ts70 = consume ts65 "}"
  in
    (
      ts70,
      [
        StrNode "func",
        StrNode fnName,
        ListNode args,
        ListNode body
      ]
    )
  
iterParseTopStmts :: [TToken] -> ([TToken], [TNode])
iterParseTopStmts ts =
  case ts of
    [] -> (ts, [])
    t : _ ->
      case (tokenVal t) of
        "func" ->
          let (ts20, fnDef) = parseFuncDef ts
              (ts30, rest) = iterParseTopStmts ts20
          in
            (ts30, ListNode fnDef : rest)
        _ -> error "invalid top statement"

parseTopStmts :: [TToken] -> [TNode]
parseTopStmts ts =
  let (_, fnDefs) = iterParseTopStmts ts
  in
    StrNode "top_stmts" : fnDefs
  
parse :: String -> String
parse src =
  let ts = parseTokens src
  in
    Json.toPrettyJson (parseTopStmts ts)

parseTokens :: String -> [TToken]
parseTokens src =
  let lineToToken line =
        case Json.parse line of
          [IntNode lineno, StrNode kind, StrNode val] ->
            case kind of
              "kw"    -> Kw    val lineno
              "ident" -> Ident val lineno
              "sym"   -> Sym   val lineno
              "int"   -> Int   val lineno
              "str"   -> Str   val lineno
              _ -> error ("must_not_happen (" ++ (show line) ++ ")")
          _ -> error "must_not_happen"
  in
    map lineToToken (lines src)
