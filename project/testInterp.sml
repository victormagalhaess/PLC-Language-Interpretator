use "Plc.sml";
exception testError;

let 
    val test = eval (fromString "x = false") [("x", IntV 123)]
in
    print("ERROR => Impossible were supposed to be raised\n");
    raise testError
end handle Impossible => print ("PASSED => Impossible were succesfully raised\n");

let 
    val test = eval (fromString "hd ([Bool] [])") []
in
    print("ERROR => HDEmptySeq were supposed to be raised\n");
    raise testError
end handle HDEmptySeq => print ("PASSED => HDEmptySeq were succesfully raised\n");

let 
    val test = eval (fromString "tl ([Bool] [])") []
in
    print("ERROR => TLEmptySeq were supposed to be raised\n");
    raise testError
end handle TLEmptySeq => print ("PASSED => TLEmptySeq were succesfully raised\n");

let 
    val test = eval (fromString "match x with | true -> 1 end") [("x", BoolV false)]
in
    print("ERROR => ValueNotFoundInMatch were supposed to be raised\n");
    raise testError
end handle ValueNotFoundInMatch => print ("PASSED => ValueNotFoundInMatch were succesfully raised\n");

let 
    val test = eval (fromString "var x = false; x(false)") []
in
    print("ERROR => NotAFunc were supposed to be raised\n");
    raise testError
end handle NotAFunc => print ("PASSED => NotAFunc were succesfully raised\n");


print("SUCCESS!\n")