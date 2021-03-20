use "Plc.sml";
exception testError;

let
    val test = teval (fromString "1::2::t") [("t", IntT)]
in
    print("ERROR => UnknownError were supposed to be raised\n");
    raise testError
end handle UnknownType => print ("PASSED => UnknownError were succesfully raised\n");

let
    val test = teval (fromString "(Int [])") [];
in
    print("ERROR => EmptySeq were supposed to be raised\n");
    raise testError
end handle EmptySeq => print ("PASSED => EmptySeq were succesfully raised\n");

let
    val test = teval (fromString "match x with | 0 -> true | _ -> 1 end") [("x", IntT)]
in
    print("ERROR => MatchResTypeDiff were supposed to be raised\n");
    raise testError
end handle MatchResTypeDiff => print ("PASSED => MatchResTypeDiff were succesfully raised\n");

let
    val test = teval (fromString "match x with | true -> 1| _ -> -1 end") [("x", IntT)]
in
    print("ERROR => MatchCondTypesDiff were supposed to be raised\n");
    raise testError
end handle MatchCondTypesDiff => print ("PASSED => MatchCondTypesDiff were succesfully raised\n");

let
    val test = teval (fromString "if 5 then 8 else 3") []
in
    print("ERROR => IfCondNotBool were supposed to be raised\n");
    raise testError
end handle IfCondNotBool => print ("PASSED => IfCondNotBool were succesfully raised\n");

let
    val test = teval (fromString "if true then 8 else false") []
in
    print("ERROR => DiffBrTypes were supposed to be raised\n");
    raise testError
end handle DiffBrTypes => print ("PASSED => DiffBrTypes were succesfully raised\n");

let
    val test = teval (fromString "(6,false)[3]") []
in
    print("ERROR => ListOutOfRange were supposed to be raised\n");
    raise testError
end handle ListOutOfRange => print ("PASSED => ListOutOfRange were succesfully raised\n");

let
    val test = teval (fromString "if true != 0 then 0 else m; f(5, 8)") [];
in
    print("ERROR => NotEqTypes were supposed to be raised\n");
    raise testError
end handle NotEqTypes => print ("PASSED => NotEqTypes were succesfully raised\n");

let
    val test = teval (fromString "fun rec f(Int n, Int m):Bool = if n != 0 then 0 else m; f(5, 8)") [];
in
    print("ERROR => WrongRetType were supposed to be raised\n");
    raise testError
end handle WrongRetType => print ("PASSED => WrongRetType were succesfully raised\n");

let
    val test = teval (fromString "fun f(Int n, Int m) = if n != 0 then 0 else m; f(true, 8)") [];
in
    print("ERROR => CallTypeMisM were supposed to be raised\n");
    raise testError
end handle CallTypeMisM => print ("PASSED => CallTypeMisM were succesfully raised\n");

let
    val test = teval (fromString "var x = 3; x(1)") []
in
    print("ERROR => NotFunc were supposed to be raised\n");
    raise testError
end handle NotFunc => print ("PASSED => NotFunc were succesfully raised\n");

let
    val test = teval (fromString "var x = 3; x[1]") []
in
    print("ERROR => OpNonList were supposed to be raised\n");
    raise testError
end handle OpNonList => print ("PASSED => OpNonList were succesfully raised\n");

print("SUCCESS!\n")