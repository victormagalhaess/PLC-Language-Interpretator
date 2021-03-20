use "Plc.sml";
exception testError;

(*Test cases given by the professor to check the correctness of the typeChecking*)
(*Should NOT raise exceptions*)
teval(If(Prim2("=", ConI 11, ConI 12), ConI 1, ConI 0))[];
teval(Let("b", Prim2("=", ConI 1, ConI 2), If(Var "b", ConI 3, ConI 4)))[];
teval(Letrec("f1",IntT,"x",IntT,Prim2 ("+",Var "x",ConI 1),Call (Var "f1",ConI 12)))[];

(*Should raise exceptions*)
let
    val test = teval(Let("b", Prim2("=", ConI 1, ConI 2),If(Var "b", Var "b", ConI 6)))[];
in 
    print("ERROR => DiffBrTypes were supposed to be raised\n");
    raise testError
end handle DiffBrTypes => print ("PASSED => DiffBrTypes were succesfully raised\n");

let
    val test = teval(Let("f",Anon (BoolT,"x",If (Var "x",ConI 11,ConI 22)),Call (Var "f",ConI 0)))[];
in
    print("ERROR => CallTypeMisM were supposed to be raised\n");
    raise testError
end handle CallTypeMisM => print ("PASSED => CallTypeMisM were succesfully raised\n"); 

let 
    val teste = teval(Letrec("f",BoolT,"x",BoolT,If (Var "x",ConI 11,ConI 22), Call (Var "f",ConB true)))[];
in
    print("ERROR => WrongRetType were supposed to be raised\n");
    raise testError
end handle WrongRetType => print ("PASSED => WrongRetType were succesfully raised\n");

(*Test cases created by the students to check the correctness of the typeChecking*)
let
    val test = teval (fromString "(Bool [])") [];
in
    print("ERROR => EmptySeq were supposed to be raised\n");
    raise testError
end handle EmptySeq => print ("PASSED => EmptySeq were succesfully raised\n");

let
    val test = teval (fromString "(1::2)") [("t", IntT)]
in
    print("ERROR => UnknownType were supposed to be raised\n");
    raise testError
end handle UnknownType => print ("PASSED => UnknownType were succesfully raised\n");

let
    val test = teval (fromString "true != 1") [];
in
    print("ERROR => NotEqTypes were supposed to be raised\n");
    raise testError
end handle NotEqTypes => print ("PASSED => NotEqTypes were succesfully raised\n");

let
    val test = teval (fromString "fun rec f(Bool x):Int = if x != true then false else x; f(false)") [];
in
    print("ERROR => WrongRetType were supposed to be raised\n");
    raise testError
end handle WrongRetType => print ("PASSED => WrongRetType were succesfully raised\n");

let
    val test = teval (fromString "fun f(Bool x) = if x != true then 0 else x; f(false)") []
in
    print("ERROR => DiffBrTypes were supposed to be raised\n");
    raise testError
end handle DiffBrTypes => print ("PASSED => DiffBrTypes were succesfully raised\n");

let
    val test = teval (fromString "if () then true else false") []
in
    print("ERROR => IfCondNotBool were supposed to be raised\n");
    raise testError
end handle IfCondNotBool => print ("PASSED => IfCondNotBool were succesfully raised\n");

let
    val test = teval (fromString "match 3 with end") [];
in
    print("ERROR => NoMatchResults were supposed to be raised\n");
    raise testError
end handle NoMatchResults => print ("PASSED => NoMatchResults were succesfully raised\n");

let
    val test = teval (fromString "match x with | 1 -> 0 | _ -> false end") [("x", IntT)]
in
    print("ERROR => MatchResTypeDiff were supposed to be raised\n");
    raise testError
end handle MatchResTypeDiff => print ("PASSED => MatchResTypeDiff were succesfully raised\n");

let
    val test = teval (fromString "match x with | 1 -> true | 0 -> false end") [("x", BoolT)]
in
    print("ERROR => MatchCondTypesDiff were supposed to be raised\n");
    raise testError
end handle MatchCondTypesDiff => print ("PASSED => MatchCondTypesDiff were succesfully raised\n");

let
    val test = teval (fromString "fun f(Bool x) = if x != true then 0 else 1; f(6)") [];
in
    print("ERROR => CallTypeMisM were supposed to be raised\n");
    raise testError
end handle CallTypeMisM => print ("PASSED => CallTypeMisM were succesfully raised\n");

let
    val test = teval (fromString "var x = false; x(false)") []
in
    print("ERROR => NotFunc were supposed to be raised\n");
    raise testError
end handle NotFunc => print ("PASSED => NotFunc were succesfully raised\n");

let
    val test = teval (fromString "(1,2,3,4)[5]") []
in
    print("ERROR => ListOutOfRange were supposed to be raised\n");
    raise testError
end handle ListOutOfRange => print ("PASSED => ListOutOfRange were succesfully raised\n");

let
    val test = teval (fromString "var x = false; x[1]") []
in
    print("ERROR => OpNonList were supposed to be raised\n");
    raise testError
end handle OpNonList => print ("PASSED => OpNonList were succesfully raised\n");

print("SUCCESS!\n")