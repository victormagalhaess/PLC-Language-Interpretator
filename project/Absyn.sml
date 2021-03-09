(*Absyn*)

datatype plcType =
   IntT
  | BoolT
  | FunT of plcType * plcType
  | ListT of plcType list
  | SeqT of plcType;

datatype expr =
   ConI of int
  | ConB of bool
  | ESeq of plcType
  | Var of string
  | Let of string * expr * expr
  | Letrec of string * plcType * string * plcType * expr * expr
  | Prim1 of string * expr
  | Prim2 of string * expr * expr
  | If of expr * expr * expr
  | Match of expr * (expr option * expr) list
  | Call of expr * expr
  | List of expr list
  | Item of int * expr
  | Anon of plcType * string * expr;


datatype plcVal =
   BoolV of bool
  | IntV of int
  | ListV of plcVal list
  | SeqV of plcVal list
  | Clos of string * string * expr * plcVal env;

(* Convert a list into a string *)
fun list2string (conv, l) =
  case l of
      [] => ""
    | h::ts => conv(h) ^ ", " ^ list2string (conv, ts);

(* Convert a plcType into a string *)
fun type2string t =
  case t of
      BoolT => "Bool"
    | IntT => "Int"
    | ListT [] => "Nil"
    | ListT ts => "(" ^ list2string (type2string, ts) ^ ")"
    | SeqT t1 => "[" ^ type2string t1 ^ "]"
    | FunT (t1,t2) =>
        case t1 of
            FunT _ => "(" ^ type2string t1 ^ ") -> " ^ type2string t2
          | _ => type2string t1 ^ " -> " ^ type2string t2;

(* Convert a plcVal into a string *)
fun val2string v =
  case v of
      BoolV true => "true"
    | BoolV false => "false"
    | IntV n => Int.toString n
    | ListV vs => "(" ^ list2string (val2string, vs) ^ ")"
    | SeqV vs => "[" ^ list2string (val2string, vs) ^ "]"
    | Clos _ => "<fun>";
