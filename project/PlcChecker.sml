(* PlcChecker *)
exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun teval (ConI _) _ = IntT
    | teval (ConB _) _ = BoolT
    | teval (Var variable) (env:plcType env) = lookup env variable
    | teval (List list) (env:plcType env) =
        let
            fun validateList (head::[]) = (teval head env)::[]
                | validateList (head::tail) = (teval head env)::validateList tail
                | validateList _ = []
            val listType = validateList list
        in
            ListT listType
        end
    | teval (ESeq sequence) _ =
        let in
            case sequence of
                SeqT sequenceType => SeqT sequenceType
            | _ => raise EmptySeq
        end
    | teval (Item (index, exp)) (env:plcType env) =
        let
            fun validateElement (i, []) = raise ListOutOfRange
                | validateElement (i, (head::[])) = if i = 1 then head else raise ListOutOfRange
                | validateElement (i, (head::tail)) = if i = 1 then head else validateElement (i - 1, tail)
            val vType = teval exp env
        in
            case vType of
                ListT listType => validateElement(index, listType)
            | _ => raise OpNonList
        end
    | teval (Let(var, exp1, exp2)) (env:plcType env) =
        let
            val exp1Type = teval exp1 env
            val nEnv = (var, exp1Type) :: env
        in
            teval exp2 nEnv
        end
    | teval (Anon(typ, arg, exp)) (env:plcType env) = 
        let
            val nEnv = (arg, typ) :: env
            val expType = teval exp nEnv
        in
            FunT (typ, expType)
        end
    | teval (Call(exp2, exp1)) (env:plcType env) =
        let
            val exp1Type = teval exp1 env
            val exp2Type = teval exp2 env
        in
            case exp2Type of FunT (argType, resultType) => 
                    if exp1Type = argType then resultType else raise CallTypeMisM
            | _ => raise NotFunc
        end
    | teval (If(condition, exp1, exp2)) (env:plcType env) =
        let
            val condType = teval condition env
            val exp1Type = teval exp1 env
            val exp2Type = teval exp2 env
        in
            case condType of
                BoolT => if exp1Type = exp2Type then exp1Type else raise DiffBrTypes
            | _ => raise IfCondNotBool
        end
    | teval (Prim1(oper, exp)) (env:plcType env) =
        let
            val expType = teval exp env
        in
            case oper of "!" => if expType = BoolT then BoolT else raise UnknownType
            | "-" => if expType = IntT then IntT else raise UnknownType
            | "hd" => let in
                    case expType of
                        SeqT sequenceType => sequenceType
                    | _ => raise UnknownType
                end
            | "tl" => let in
                    case expType of
                        SeqT sequenceType => SeqT sequenceType
                    | _ => raise UnknownType
                end
            | "ise" => let in
                    case expType of
                        SeqT sequenceType => BoolT
                    | _ => raise UnknownType
                end
            | "print" => ListT []
            | _ => raise UnknownType
        end
    | teval (Letrec(fName, argTyp, arg, funTyp, exp1, exp2)) (env:plcType env) =
        let
            val recEnv = (fName, FunT (argTyp, funTyp))
            val argEnv = (arg, argTyp)
            val exp1Type = teval exp1 (recEnv :: argEnv :: env)
            val exp2Type = teval exp2 (recEnv :: env)
        in
            if exp1Type = funTyp then exp2Type else raise WrongRetType
        end
    | teval (Match(exp1, exp2)) (env:plcType env) =
        if null exp2 then raise NoMatchResults else
            let
                val initialCond = teval exp1 env
                val pattern = (#2 (hd exp2))
                val patternType = teval pattern env
                fun find (Match(exp1, exp2)) (env:plcType env) =
                        let in
                            case exp2 of x::[] => let in
                                        case x of
                                            (SOME exp2, exp3) => 
                                                if (teval exp3 env) = patternType then
                                                    if initialCond = (teval exp2 env) then 
                                                        teval exp3 env 
                                                    else raise MatchCondTypesDiff
                                                else raise MatchResTypeDiff
                                        | (NONE, exp3) => if (teval exp3 env) = patternType then patternType else raise MatchResTypeDiff
                                    end
                            | x::xs => let in
                                    case x of (SOME exp2, exp3) => 
                                            if (teval exp3 env) = patternType then
                                                if initialCond = (teval exp2 env) then
                                                    find (Match(exp1, xs)) env 
                                                else raise MatchCondTypesDiff
                                            else raise MatchResTypeDiff
                                    | _ => raise UnknownType
                                end
                        end
                    | find _ _ = raise UnknownType
            in
                find (Match(exp1, exp2)) env
            end
    | teval (Prim2(oper, exp1, exp2)) (env:plcType env) =
        let
            val exp1Type = teval exp1 env
            val exp2Type = teval exp2 env
        in
            case oper of "&&" => if exp1Type = BoolT andalso exp2Type = BoolT then BoolT else raise UnknownType
            | "::" => let in
                    case (exp1Type, exp2Type) of (IntT, ListT []) => SeqT IntT
                    | (IntT, SeqT sequenceType) => if sequenceType = IntT then SeqT sequenceType else raise NotEqTypes
                    | (BoolT, ListT []) => SeqT BoolT
                    | (BoolT, SeqT sequenceType) => if sequenceType = BoolT then SeqT sequenceType else raise NotEqTypes
                    | (ListT t, ListT []) => SeqT (ListT t)
                    | (ListT t, SeqT sequenceType) => if sequenceType = ListT t then SeqT sequenceType else raise NotEqTypes
                    | _ => raise UnknownType
                end
            | "<" => if exp1Type = IntT andalso exp2Type = IntT then BoolT else raise UnknownType
            | "<=" => if exp1Type = IntT andalso exp2Type = IntT then BoolT else raise UnknownType
            | "=" => if exp1Type = exp2Type andalso (exp1Type = IntT orelse exp1Type = BoolT) then BoolT else raise NotEqTypes
            | "!=" => if exp1Type = exp2Type andalso (exp1Type = IntT orelse exp1Type = BoolT) then BoolT else raise NotEqTypes
            | "+" => if exp1Type = IntT andalso exp2Type = IntT then IntT else raise UnknownType
            | "-" => if exp1Type = IntT andalso exp2Type = IntT then IntT else raise UnknownType
            | "*" => if exp1Type = IntT andalso exp2Type = IntT then IntT else raise UnknownType
            | "/" => if exp1Type = IntT andalso exp2Type = IntT then IntT else raise UnknownType
            | ";" => exp2Type
            | _ => raise UnknownType
        end