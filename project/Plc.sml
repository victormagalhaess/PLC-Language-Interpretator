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
        handle SymbolNotFound => "SymbolNotFound: a symbol was not defined or could not be found"
        | NotEqTypes =>  "NotEqTypes: comparison with different types are not allowed"
        | Impossible => "Impossible: this error should not happen"
        | NoMatchResults =>  "NoMatchResults: match was unable to match with this empty pattern"
        | WrongRetType =>  "WrongRetType: function returning a different type from its declaration is not allowed"
        | MatchResTypeDiff =>  "MatchResTypeDiff: match with different result types is not allowed"
        | NotFunc =>  "NotFunc: calling non-functions as functions is not allowed"
        | EmptySeq =>  "EmptySeq: sequences without type are not allowed"
        | ValueNotFoundInMatch =>  "ValueNotFoundInMatch: match was unable to match with this pattern"
        | MatchCondTypesDiff =>  "MatchCondTypesDiff: matches with different condition types are not allowed"
        | UnknownType => "UnknownType: the interpreter could not resolve this type"
        | DiffBrTypes =>  "DiffBrTypes: if branches with different types are not allowed"
        | NotAFunc =>  "NotAFunc: treating non-function types as functions is not allowed"
        | ListOutOfRange =>  "ListOutOfRange: accessing elements out of a list is not allowed"
        | CallTypeMisM =>  "CallTypeMisM: calling a function with incorrect parameters types is not allowed"
        | IfCondNotBool =>  "IfCondNotBool: ifs using non booleans as conditions are not allowed"
        | OpNonList =>  "OpNonList: treat something that is not a list as a list is not allowed"
        | TLEmptySeq =>  "TLEmptySeq: accessing the tail of empty sequence is not allowed"
        | HDEmptySeq =>  "HDEmptySeq: accessing the head of empty sequence is not allowed"
        | _ => "UnknownError: the interpreter is unable to figure out what caused this error"