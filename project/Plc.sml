(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run exp =
        let
            val expType = teval exp []
            val expResult = eval exp []
        in
            val2string(expResult) ^ " : " ^ type2string(expType)
        end
        handle SymbolNotFound => let val p = print ("SymbolNotFound: a symbol was not defined or could not be found") in raise SymbolNotFound end
        | NotEqTypes => let val p =  print ("NotEqTypes: comparison with different types are not allowed") in raise NotEqTypes end
        | Impossible => let val p = print ("Impossible: this error should not happen") in raise Impossible end
        | NoMatchResults => let val p =  print ("NoMatchResults: match was unable to match with this empty pattern") in raise NoMatchResults end
        | WrongRetType => let val p =  print ("WrongRetType: function returning a different type from its declaration is not allowed") in raise WrongRetType end
        | MatchResTypeDiff => let val p =  print ("MatchResTypeDiff: match with different result types is not allowed") in raise MatchResTypeDiff end
        | NotFunc => let val p =  print ("NotFunc: calling non-functions as functions is not allowed") in raise NotFunc end
        | EmptySeq => let val p =  print ("EmptySeq: sequences without type are not allowed") in raise EmptySeq end
        | ValueNotFoundInMatch => let val p =  print ("ValueNotFoundInMatch: match was unable to match with this pattern") in raise ValueNotFoundInMatch end
        | MatchCondTypesDiff => let val p =  print ("MatchCondTypesDiff: matches with different condition types are not allowed") in raise MatchCondTypesDiff end
        | UnknownType => let val p = print ("UnknownType: the interpreter could not resolve this type") in raise UnknownType end
        | DiffBrTypes => let val p =  print ("DiffBrTypes: if branches with different types are not allowed") in raise DiffBrTypes end
        | NotAFunc => let val p =  print ("NotAFunc: treating non-function types as functions is not allowed") in raise NotAFunc end
        | ListOutOfRange => let val p =  print ("ListOutOfRange: accessing elements out of a list is not allowed") in raise ListOutOfRange end
        | CallTypeMisM => let val p =  print ("CallTypeMisM: calling a function with incorrect parameters types is not allowed") in raise CallTypeMisM end
        | IfCondNotBool => let val p =  print ("IfCondNotBool: ifs using non booleans as conditions are not allowed") in raise IfCondNotBool end
        | OpNonList => let val p =  print ("OpNonList: treat something that is not a list as a list is not allowed") in raise OpNonList end
        | TLEmptySeq => let val p =  print ("TLEmptySeq: accessing the tail of empty sequence is not allowed") in raise TLEmptySeq end
        | HDEmptySeq => let val p =  print ("HDEmptySeq: accessing the head of empty sequence is not allowed") in raise HDEmptySeq end
        | _ => let val p = print ("UnknownError: the interpreter is unable to figure out what caused this error") in raise Impossible end