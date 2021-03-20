(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (ConI integer) _ = IntV integer
    | eval (ConB boolean) _ = BoolV boolean
    | eval (ESeq emptySequence) _ = SeqV []
    | eval (Var variable) (env:plcVal env) = lookup env variable
    | eval (Item (index, exp)) (env:plcVal env) =
        let
            fun getElementI (index, []) = raise Impossible
                | getElementI (index, (x::[])) = if index = 1 then x else raise Impossible
                | getElementI (index, (x::xs)) = if index = 1 then x else getElementI (index - 1, xs)
            val value = eval exp env
        in
            case value of ListV list => getElementI (index, list)
            | SeqV sequence => getElementI (index, sequence)
            | _ => raise Impossible
        end
    | eval (Prim1 (oper, exp)) (env:plcVal env) =
        let
            val value = eval exp env
        in
            case value of IntV integer => 
                    let in
                        case oper of
                            "-" => IntV (~ integer)
                        | "print" => 
                            let 
                                val value = IntV integer
                                val ignore = print(val2string(value) ^ "\n")
                            in
                                ListV []
                            end
                        | _ => raise Impossible
                    end
            | BoolV boolean =>
                let in
                    case oper of "!" => BoolV (not boolean)
                    | "print" => 
                        let 
                            val value = BoolV boolean
                            val ignore = print(val2string(value) ^ "\n")
                        in
                            ListV []
                        end
                    | _ => raise Impossible
                end
            | SeqV sequence =>
                let in
                    case oper of "hd" => let in let in hd sequence end handle Empty => raise HDEmptySeq end
                    | "tl" => let in let in SeqV (tl sequence) end handle Empty => raise TLEmptySeq end
                    | "ise" =>
                        let in
                            case sequence of [] => BoolV true
                            | _ => BoolV false
                        end
                    | "print" => 
                        let 
                            val ignore = print(list2string(val2string, sequence) ^ "\n")
                        in
                            ListV []
                        end
                    | _ => raise Impossible
                end
            | ListV list =>
                let in
                    case oper of "print" => 
                            let 
                                val ignore = print(list2string(val2string, list) ^ "\n")
                            in
                                ListV []
                            end
                    | _ => raise Impossible
                end
            | _ => raise Impossible
        end
    | eval (Prim2 (oper, exp1, exp2)) (env:plcVal env) =
        if oper = ";" then
            let
                val ignore = eval exp1 env
            in
                eval exp2 env
            end
        else
            let
                val value1 = eval exp1 env
                val value2 = eval exp2 env
            in
                case (value1, value2) of
                    (IntV integer1, IntV integer2) => 
                        let in
                            case oper of "+" => IntV (integer1 + integer2)
                            | "-" => IntV (integer1 - integer2)
                            | "*" => IntV (integer1 * integer2)
                            | "/" => IntV (integer1 div integer2)
                            | "<" => BoolV (integer1 < integer2)
                            | "<=" => BoolV (integer1 <= integer2)
                            | "=" => BoolV (integer1 = integer2)
                            | "!=" => BoolV (integer1 <> integer2)
                            | _ => raise Impossible
                        end
                | (BoolV boolean1, BoolV boolean2) => 
                    let in
                        case oper of "&&" => BoolV (boolean1 andalso boolean2)
                        | "=" => BoolV (boolean1 = boolean2)
                        | "!=" => BoolV (boolean1 <> boolean2)
                        | _ => raise Impossible
                    end
                | (IntV integer1, SeqV sequence) => 
                    let in
                        case oper of
                            "::" => SeqV (IntV integer1 :: sequence)
                        | _ => raise Impossible
                    end
                | (BoolV boolean1, SeqV sequence) => 
                    let in
                        case oper of
                            "::" => SeqV (BoolV boolean1 :: sequence)
                        | _ => raise Impossible
                    end
                | (ListV list1, SeqV sequence) => 
                    let in
                        case oper of
                            "::" => SeqV (ListV list1 :: sequence)
                        | _ => raise Impossible
                    end
                | _ => raise Impossible
            end
    | eval (Match (exp1, matchList)) (env:plcVal env) = 
        let 
            val evalMatchVar = eval exp1 env 
            fun checkMatch (matchVar, x::[]) env =
                    let in
                        case x of (SOME exp2, exp3) => if matchVar = eval exp2 env then exp3 else raise ValueNotFoundInMatch
                        | (NONE, exp3) => exp3
                    end
                | checkMatch (matchVar, x::xs) env =  let in
                        case x of (SOME exp2, exp3) => if matchVar = eval exp2 env then exp3 else checkMatch (matchVar, xs) env
                        | (NONE, exp3) => raise Impossible
                    end
                | checkMatch (matchVar, _ ) env = raise Impossible
        in
            eval (checkMatch (evalMatchVar, matchList) env) env
        end
    | eval (Let (var, exp1, exp2)) (env:plcVal env) =
        let
            val nEnv = (var, eval exp1 env) :: env
        in
            eval exp2 nEnv
        end
    | eval (Anon (typ, arg, exp)) (env:plcVal env) = Clos ("", arg, exp, env) (* We need to check if var can be found in the env of Anon *)
    | eval (Call (exp1, exp2)) (env:plcVal env) = 
        let
            fun getArgs (List (x::[])) = [eval x env]
                | getArgs (List (x::xs)) = [eval x env] @ getArgs (List xs)
                | getArgs (exp) = [eval exp env]
            val nEnv = [("$list", ListV (getArgs exp2))] @ env
            val f = eval exp1 env
        in
            case f of
                Clos(name, var, exp, cEnv) =>
                    let
                        val ev = eval exp2 nEnv
                        val fEnv = (var, ev)::(name, f)::cEnv
                    in
                        eval exp fEnv
                    end
            | _ => raise NotAFunc
        end
    | eval (Letrec (fName, argTyp, arg, funTyp, exp1, exp2)) (env:plcVal env) =
        let
            val nEnv = (fName, Clos(fName, arg, exp1, env)) :: env
        in
            eval exp2 nEnv
        end
    | eval (List []) (env:plcVal env) = ListV []
    | eval (List list) (env:plcVal env) = 
        let
            fun unroll (head::[]) = eval head env :: []
                | unroll (head::tail) = eval head env :: unroll tail
                | unroll _ = raise Impossible;
        in
            ListV (unroll list)
        end
    | eval (If (exp1, exp2, exp3)) (env:plcVal env) = 
        let in
            case eval exp1 env of 
                BoolV true => eval exp2 env
            | BoolV false => eval exp3 env
            | _ => raise Impossible
        end
;