use "Plc.sml";
exception testError;

let 
    val test = eval (fromString "3::7::t") [("t", IntV 19)]
in
    print("ERROR => Impossible were supposed to be raised\n");
    raise testError
end handle Impossible => print ("PASSED => Impossible were succesfully raised\n");

let 
    val test = eval (fromString "hd ([Int] [])") []
in
    print("ERROR => HDEmptySeq were supposed to be raised\n");
    raise testError
end handle HDEmptySeq => print ("PASSED => HDEmptySeq were succesfully raised\n");

let 
    val test = eval (fromString "tl ([Int] [])") []
in
    print("ERROR => TLEmptySeq were supposed to be raised\n");
    raise testError
end handle TLEmptySeq => print ("PASSED => TLEmptySeq were succesfully raised\n");

let 
    val test = eval (fromString "var x = 3; x(1)") []
in
    print("ERROR => NotAFunc were supposed to be raised\n");
    raise testError
end handle NotAFunc => print ("PASSED => NotAFunc were succesfully raised\n");

let 
    val test = eval (fromString "match x with | 0 -> 1 end") [("x", IntV 3)]
in
    print("ERROR => ValueNotFoundInMatch were supposed to be raised\n");
    raise testError
end handle ValueNotFoundInMatch => print ("PASSED => ValueNotFoundInMatch were succesfully raised\n");

print("SUCCESS!\n")