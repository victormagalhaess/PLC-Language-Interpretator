%%

%name PlcParser

%pos int

%term VAR | END | FN | RECURSIVE
    | IF | THEN | ELSE
    | MATCH | WITH
    | NEGATION | AND
    | HEAD | TAIL | ISEMPTY
    | PRINT
    | PLUS | MINUS | MULTI | DIV 
    | EQ | DIF | SMALLER | SMALLEREQ 
    | DOUBLEPTS | COLON | SEMIC | COMMA | ARROW | VERTBAR | UNDERSCORE | FUNARROW
    | NIL | BOOL | INT
    | TRUE | FALSE
    | LPAR | RPAR | RKEY | LKEY | RBRACK | LBRACK
    | NAME of string | CINT of int | FUN
    | EOF

%nonterm Prog of expr 
    | Decl of expr
    | Expr of expr
    | AtomExpr of expr
    | AppExpr of expr
    | Const of expr
    | Comps of expr list
    | MatchExpr of (expr option * expr) list 
    | CondExpr of expr option
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | AtomType of plcType
    | Types of plcType list

%eop EOF

%right SEMIC ARROW
%nonassoc IF
%left ELSE 
%left AND 
%left EQ DIF
%left SMALLER SMALLEREQ
%right DOUBLEPTS
%left PLUS MINUS
%left MULTI DIV
%nonassoc NEGATION HEAD TAIL ISEMPTY PRINT NAME
%left LBRACK

%noshift EOF
%start Prog

%%

Prog: Expr (Expr) 
    | Decl (Decl)

Decl: VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
    | FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, makeAnon(Args, Expr), Prog))
    | FUN RECURSIVE NAME Args COLON Type EQ Expr SEMIC Prog (makeFun(NAME, Args, Type, Expr, Prog))

Expr: AtomExpr(AtomExpr)
    | AppExpr(AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match (Expr, MatchExpr))
    | NEGATION Expr (Prim1("!", Expr))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | HEAD Expr (Prim1("hd", Expr))
    | TAIL Expr (Prim1("tl", Expr))
    | ISEMPTY Expr (Prim1("ise", Expr))
    | PRINT Expr (Prim1("print", Expr))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULTI Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | MINUS Expr (Prim1("-", Expr))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr DIF Expr (Prim2("!=", Expr1, Expr2))
    | Expr SMALLER Expr (Prim2("<", Expr1, Expr2))
    | Expr SMALLEREQ Expr (Prim2("<=", Expr1, Expr2))
    | Expr DOUBLEPTS Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | Expr LBRACK CINT RBRACK (Item (CINT, Expr))

AtomExpr: Const (Const)
    | NAME (Var(NAME))
    | LKEY Prog RKEY (Prog)
    | LPAR Comps RPAR (List Comps)
    | LPAR Expr RPAR (Expr)
    | FN Args FUNARROW Expr END (makeAnon(Args, Expr))

AppExpr: AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
    | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const: TRUE (ConB true) | FALSE (ConB false)
    | CINT (ConI CINT)
    | LPAR RPAR (List [])
    | LPAR Type LBRACK RBRACK RPAR (ESeq(Type))

Comps: Expr COMMA Expr (Expr1 :: Expr2 :: [])
    | Expr COMMA Comps (Expr :: Comps)

MatchExpr: END ([])
    | VERTBAR CondExpr ARROW Expr MatchExpr ((CondExpr, Expr) :: MatchExpr)

CondExpr: Expr (SOME(Expr))
    | UNDERSCORE (NONE)

Args: LPAR RPAR ([])
    | LPAR Params RPAR (Params)

Params : TypedVar (TypedVar::[])
    | TypedVar COMMA Params (TypedVar::Params)

TypedVar: Type NAME ((Type, NAME))

Type: AtomType(AtomType)
    | LPAR Types RPAR (ListT Types)
    | LBRACK Type RBRACK (SeqT Type)
    | Type ARROW Type (FunT (Type1, Type2))

AtomType: NIL (ListT [])
    | BOOL (BoolT)
    | INT (IntT)
    | LPAR Type RPAR (Type)

Types: Type COMMA Type (Type1 :: Type2 :: [])
    | Type COMMA Types (Type :: Types)