use "Plc.sml";
exception testError;

let
    val test = teval (fromString "x") [];
in
    print("ERROR => SymbolNotFound were supposed to be raised\n");
    raise testError
end handle SymbolNotFound => print ("PASSED => SymbolNotFound were succesfully raised\n");

print("SUCCESS\n");