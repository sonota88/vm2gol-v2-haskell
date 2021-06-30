import Types
import qualified Json as Json

data TEnv = Env {
  lvarNames :: [String],
  fnArgNames :: [String],
  labelId :: Int
  }
  deriving Show

main :: IO ()
main = do
  src <- getContents
  putStr $ codegen src

-- --------------------------------

nameIndex :: [String] -> String -> Maybe Int
nameIndex names name =
  let iter _names _name i =
        case _names of
          [] -> Nothing
          hd : tl -> 
            if hd == _name then
              Just i
            else
              iter tl _name (i + 1)
  in
    iter names name 0

lvarDisp :: [String] -> String -> Int
lvarDisp names name =
  case (nameIndex names name) of
    Just i -> -(i + 1)
    Nothing -> error "must not happen"

fnArgDisp :: [String] -> String -> Int
fnArgDisp names name =
  case (nameIndex names name) of
    Just i -> i + 2
    Nothing -> error "must not happen"

asmPrologue :: String
asmPrologue = "  push bp\n"
              ++ "  cp sp bp\n"

asmEpilogue :: String
asmEpilogue = "  cp bp sp\n"
              ++ "  pop bp\n"

-- --------------------------------

_genExprAdd :: String
_genExprAdd = "  pop reg_b\n"
              ++ "  pop reg_a\n"
              ++ "  add_ab\n"

_genExprMult :: String
_genExprMult = "  pop reg_b\n"
               ++ "  pop reg_a\n"
               ++ "  mult_ab\n"

_genExprEq :: TEnv -> (TEnv, String)
_genExprEq env =
  let lidStr = show (labelId env)
      env50 = env { labelId = (labelId env) + 1 }
  in
    (
      env50,
      "  pop reg_b\n"
      ++ "  pop reg_a\n"
      ++ "  compare\n"
      ++ "  jump_eq then_" ++ lidStr ++ "\n"
      ++ "  cp 0 reg_a\n"
      ++ "  jump end_eq_" ++ lidStr ++ "\n"
      ++ "label then_" ++ lidStr ++ "\n"
      ++ "  cp 1 reg_a\n"
      ++ "label end_eq_" ++ lidStr ++ "\n"
    )

_genExprNeq :: TEnv -> (TEnv, String)
_genExprNeq env =
  let lidStr = show (labelId env)
      env50 = env { labelId = (labelId env) + 1 }
  in
    (
      env50,
      "  pop reg_b\n"
      ++ "  pop reg_a\n"
      ++ "  compare\n"
      ++ "  jump_eq then_" ++ lidStr ++ "\n"
      ++ "  cp 1 reg_a\n"
      ++ "  jump end_neq_" ++ lidStr ++ "\n"
      ++ "label then_" ++ lidStr ++ "\n"
      ++ "  cp 0 reg_a\n"
      ++ "label end_neq_" ++ lidStr ++ "\n"
    )

_genExprBinany :: TEnv -> [TNode] -> (TEnv, String)
_genExprBinany env expr =
  case expr of
    [op, lhs, rhs] ->
      let (_, asmLhs) = genExpr env lhs
          (_, asmRhs) = genExpr env rhs
          (env50, asmExpr) =
            case op of StrNode "+"  -> (env, _genExprAdd)
                       StrNode "*"  -> (env, _genExprMult)
                       StrNode "==" -> _genExprEq env
                       StrNode "!=" -> _genExprNeq env
                       _ -> error "unknown operator"
      in
        (
          env50,
          asmLhs
          ++ "  push reg_a\n"
          ++ asmRhs
          ++ "  push reg_a\n"
          ++ asmExpr
        )
    _ -> error "unexpected pattern"

genExpr :: TEnv -> TNode -> (TEnv, String)
genExpr env expr =
  case expr of
    IntNode n -> (env, "  cp " ++ (show n) ++ " reg_a\n")
    StrNode s ->
      if elem s (lvarNames env) then
        let disp = lvarDisp (lvarNames env) s
        in
          (env, "  cp " ++ "[bp:" ++ (show disp) ++ "]" ++ " reg_a\n")
      else if elem s (fnArgNames env) then
        let disp = fnArgDisp (fnArgNames env) s
        in
          (env, "  cp " ++ "[bp:" ++ (show disp) ++ "]" ++ " reg_a\n")
      else
        error ("name lookup failed (" ++ s ++ ")")
    ListNode xs -> _genExprBinany env xs

_genSet :: TEnv -> String -> TNode -> (TEnv, String)
_genSet env varName expr =
  if elem varName (lvarNames env) then
    let disp = lvarDisp (lvarNames env) varName
        (env50, asmExpr) = genExpr env expr
    in
      (
        env50,
        asmExpr
        ++ "  cp reg_a [bp:" ++ (show disp) ++ "]\n"
      )
  else
    error ("name lookup failed (" ++ varName ++ ")")

genSet :: TEnv -> [TNode] -> (TEnv, String)
genSet env stmt =
  case stmt of
    [
      StrNode "set",
      StrNode varName,
      expr
      ] -> _genSet env varName expr
    _ -> error "unexpected pattern"

_pushFnArgs :: TEnv -> [TNode] -> String
_pushFnArgs env args =
  let iter revArgs =
        case revArgs of
          [] -> ""
          arg : rest ->
            let (_, asmExpr) = genExpr env arg
            in
              asmExpr
              ++ "  push reg_a\n"
              ++ (iter rest)
  in
    iter (reverse args)

_genFuncall :: TEnv -> [TNode] -> String
_genFuncall env funcall =
  case funcall of
    StrNode fnName : args ->
      (_pushFnArgs env args)
      ++ (_genVmComment ("call  " ++ fnName))
      ++ "  call " ++ fnName ++ "\n"
      ++ "  add_sp " ++ (show (length args)) ++ "\n"
    _ -> error "unexpected pattern"

genCall :: TEnv -> [TNode] -> String
genCall env stmt =
  case stmt of
    StrNode "call" : funcall -> _genFuncall env funcall
    _ -> error "unexpected pattern"

genCallSet :: TEnv -> [TNode] -> String
genCallSet env stmt =
  case stmt of
    [
      StrNode "call_set",
      StrNode varName,
      ListNode funcall
      ]
      ->
      let asmFuncall = _genFuncall env funcall
      in
        if elem varName (lvarNames env) then
          let disp = lvarDisp (lvarNames env) varName
          in
            asmFuncall ++ "  cp reg_a [bp:" ++ (show disp) ++ "]\n"
        else
          error "name lookup failed"
    _ -> error "unexpected_pattern"

genReturn :: TEnv -> [TNode] -> (TEnv, String)
genReturn env stmt =
  case stmt of
    [StrNode "return", expr] -> genExpr env expr
    _ -> error "unexpected_pattern"

genWhile :: TEnv -> [TNode] -> (TEnv, String)
genWhile env stmt =
  let lidStr = (show (labelId env))
      env10 = env { labelId = ((labelId env) + 1) }
  in
    case stmt of
      [StrNode "while", expr, ListNode stmts] ->
        let (env20, asmExpr) = genExpr env10 expr
            (env40, asmStmts) = genStmts env20 stmts
        in
          (
            env40,
            "label while_" ++ lidStr ++  "\n"
            ++ asmExpr
            ++ "  cp 1 reg_b\n"
            ++ "  compare\n"
            ++ "  jump_eq true_" ++ lidStr ++  "\n"
            ++ "  jump end_while_" ++ lidStr ++  "\n"
            ++ "label true_" ++ lidStr ++  "\n"
            ++ asmStmts
            ++ "  jump while_" ++ lidStr ++  "\n"
            ++ "label end_while_" ++ lidStr ++  "\n"
          )
      _ -> error "unexpected_pattern"

_genWhenClauses :: TEnv -> [TNode] -> String -> Int -> (TEnv, String)
_genWhenClauses env whenClauses lidStr whenIdx =
  case whenClauses of
    [] -> (env, "")
    ListNode hd : rest ->
      let expr : stmts = hd
          (env20, asmExpr) = genExpr env expr
          (env40, asmStmts) = genStmts env20 stmts
          (env60, asmRest) = _genWhenClauses env40 rest lidStr (whenIdx + 1)
      in
        (
          env60,
          asmExpr
          ++ "  cp 1 reg_b\n"
          ++ "  compare\n"
          ++ "  jump_eq when_" ++ lidStr ++ "_" ++ (show whenIdx) ++ "\n"
          ++ "  jump end_when_" ++ lidStr ++ "_" ++ (show whenIdx) ++ "\n"
          ++ "label when_" ++ lidStr ++ "_" ++ (show whenIdx) ++ "\n"
          ++ asmStmts
          ++ "  jump end_case_" ++ lidStr ++ "\n"
          ++ "label end_when_" ++ lidStr ++ "_" ++ (show whenIdx) ++ "\n"
          ++ asmRest
        )
    _ -> error "must_not_happen"

genCase :: TEnv -> [TNode] -> (TEnv, String)
genCase env stmt =
  let lidStr = show (labelId env)
      env10 = env { labelId = ((labelId env) + 1) }
  in
    case stmt of
      StrNode "case" : whenClauses ->
        let (env20, asmWhen) = _genWhenClauses env10 whenClauses lidStr 0
        in
          (env20, asmWhen ++ "label end_case_" ++ lidStr ++ "\n")
      _ -> error "unexpected_pattern"

_genVmComment :: String -> String
_genVmComment cmt =
  let repl ' ' = '~'
      repl c = c
  in
    "  _cmt " ++ (map repl cmt) ++ "\n"

genVmComment :: [TNode] -> String
genVmComment stmt =
  case stmt of
    [
      StrNode "_cmt",
      StrNode cmt
      ] -> _genVmComment cmt
    _ -> error "unexpected_pattern"

genStmts :: TEnv -> [TNode] -> (TEnv, String)
genStmts env stmts =
  case stmts of
    [] -> (env, "")
    ListNode stmt : rest ->
      let (env20, asmStmt) = genStmt env stmt
          (env30, asmRest) = genStmts env20 rest
      in
        (env30, asmStmt ++ asmRest)
    _ -> error "must_not_happen"

genStmt :: TEnv -> [TNode] -> (TEnv, String)
genStmt env stmt =
  case stmt of
    StrNode "set"      : _ -> genSet env stmt
    StrNode "_cmt"     : _ -> (env, genVmComment stmt)
    StrNode "call"     : _ -> (env, genCall env stmt)
    StrNode "call_set" : _ -> (env, genCallSet env stmt)
    StrNode "return"   : _ -> genReturn env stmt
    StrNode "while"    : _ -> genWhile env stmt
    StrNode "case"     : _ -> genCase env stmt
    _ -> error "unexpected_pattern"

genVar :: TEnv -> [TNode] -> (TEnv, String)
genVar env stmt =
  case stmt of
    [] -> (env, "")
    [
      StrNode "var",
      StrNode _
      ] -> (env, "  sub_sp 1\n")
    [
      StrNode "var",
      StrNode varName,
      expr
      ] ->
      let (env50, asmSet) = _genSet env varName expr
      in
        (env50, "  sub_sp 1\n" ++ asmSet)
    _ -> error "unexpected pattern"

genFuncBody :: TEnv -> [TNode] -> (TEnv, String)
genFuncBody env stmts =
  case stmts of
    [] -> (env, "")
    ListNode stmt : rest ->
      case stmt of
        (StrNode "var" : StrNode varName : _) ->
          let env5 = env { lvarNames = ((lvarNames env) ++ [varName]) }
              (env6, asmVar) = genVar env5 stmt
              (env10, asmFn) = genFuncBody env6 rest
          in
            (env10, asmVar ++ asmFn)
        _ ->
          let (env8, stmtStr) = genStmt env stmt
              (env10, asmFn) = genFuncBody env8 rest
          in
            (env10, stmtStr ++ asmFn)
    _ -> error "must_not_happen"

genFuncDef :: TEnv -> [TNode] -> (TEnv, String)
genFuncDef env funcDef =
  case funcDef of
    [
      StrNode "func",
      StrNode fnName,
      ListNode fnArgNames,
      ListNode stmts
      ] ->
      let fans_ = map (
            \it ->
              case it of
                StrNode s -> s
                _ -> error "must not happen"
            ) fnArgNames
          env50 = env { fnArgNames = fans_, lvarNames = [] }
          asm10 = "label " ++ fnName ++ "\n"
                   ++ asmPrologue
          (env10, asm20) = genFuncBody env50 stmts
          asm40 = asm10 ++ asm20
          asm50 = asm40
                  ++ asmEpilogue
                  ++ "  ret\n"
      in
        (env10, asm50)
    _ -> error ("unexpected pattern: " ++ (Json.toPrettyJson funcDef))

iterGenTopStmts :: TEnv -> [TNode] -> (TEnv, String)
iterGenTopStmts env topStmts =
  case topStmts of
    [] -> (env, "")
    ListNode stmt : restStmts ->
      let (env20, asmFnDef) = genFuncDef env stmt
          (env50, asmRest) = iterGenTopStmts env20 restStmts
      in
        (env50, asmFnDef ++ asmRest)
    _ -> error "must not happen"

genTopStmts :: [TNode] -> String
genTopStmts topStmts =
  let (_, asmTopStmts) = iterGenTopStmts initialEnv topStmts
  in
    asmTopStmts
  where
    initialEnv = Env { lvarNames = [], fnArgNames = [], labelId = 1 }

genBuiltinSetVram :: String
genBuiltinSetVram = "label set_vram\n"
                    ++ asmPrologue
                    ++ "  set_vram [bp:2] [bp:3]\n" -- vram_addr value
                    ++ asmEpilogue
                    ++ "  ret\n"

genBuiltinGetVram :: String
genBuiltinGetVram = "label get_vram\n"
                    ++ asmPrologue
                    ++ "  get_vram [bp:2] reg_a\n" -- vram_addr dest
                    ++ asmEpilogue
                    ++ "  ret\n"

codegen :: String -> String
codegen src = "  call main\n"
              ++ "  exit\n"
              ++ (genTopStmts (tail ast))
              ++ "#>builtins\n"
              ++ genBuiltinSetVram
              ++ genBuiltinGetVram
              ++ "#<builtins\n"
  where
    ast = Json.parse src
