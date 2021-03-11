(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

fun strToInt s =
    case Int.fromString s of
    SOME i => i
    |  NONE => raise Fail ("Could not convert string '" ^ s ^ "' to integer")

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

fun keyWord (s, lpos, rpos) = 
    case s of
    "var" => VAR (lpos, rpos)
    | "end" => END (lpos, rpos)
    | "fn" => FN (lpos, rpos)
    | "rec" => RECURSIVE (lpos, rpos)
    | "if" => IF (lpos, rpos)
    | "then" => THEN (lpos, rpos)
    | "else" => ELSE (lpos, rpos)
    | "match" => MATCH (lpos, rpos)
    | "with" => WITH (lpos, rpos)
    | "hd" => HEAD (lpos, rpos)
    | "tl" => TAIL (lpos, rpos)
    | "ise" => ISEMPTY (lpos, rpos)
    | "print" => PRINT (lpos, rpos)
    | "_" => UNDERSCORE (lpos, rpos)
    | "Nil" => NIL (lpos, rpos)
    | "Bool" => BOOL (lpos, rpos)
    | "Int" => INT (lpos, rpos)
    | "true" => TRUE (lpos, rpos)
    | "false" => FALSE (lpos, rpos)
    | "fun" => FUN (lpos, rpos)
    | _ => NAME (s, lpos, rpos)

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;
%s COMMENTARY;
%%

\n => (lineNumber := !lineNumber + 1; lex());
<INITIAL>"(*" => (YYBEGIN COMMENTARY; lex());
<COMMENTARY>"*)" => (YYBEGIN INITIAL; lex());
<COMMENTARY>. => (lex());
<INITIAL>{whitespace}+ => (lex());
<INITIAL>{digit}+ => (CINT(strToInt(yytext), yypos, yypos));
<INITIAL>{identifier} => (keyWord(yytext, yypos, yypos));
<INITIAL>"!" => (NEGATION(yypos, yypos));
<INITIAL>"&&" => (AND(yypos, yypos));
<INITIAL>"+" => (PLUS(yypos, yypos));
<INITIAL>"-" => (MINUS(yypos, yypos));
<INITIAL>"*" => (MULTI(yypos, yypos));
<INITIAL>"/" => (DIV(yypos, yypos));
<INITIAL>"=" => (EQ(yypos, yypos));
<INITIAL>"!=" => (DIF(yypos, yypos));
<INITIAL>"<" => (SMALLER(yypos, yypos));
<INITIAL>"<=" => (SMALLEREQ(yypos, yypos));
<INITIAL>"::" => (DOUBLEPTS(yypos, yypos));
<INITIAL>":" => (COLON(yypos, yypos));
<INITIAL>";" => (SEMIC(yypos, yypos));
<INITIAL>"," => (COMMA(yypos, yypos));
<INITIAL>"->" => (ARROW(yypos, yypos));
<INITIAL>"|" => (VERTBAR(yypos, yypos));
<INITIAL>"=>" => (FUNARROW(yypos, yypos));
<INITIAL>"(" => (LPAR(yypos, yypos));
<INITIAL>")" => (RPAR(yypos, yypos));
<INITIAL>"{" => (LKEY(yypos, yypos));
<INITIAL>"}" => (RKEY(yypos, yypos));
<INITIAL>"[" => (LBRACK(yypos, yypos));
<INITIAL>"]" => (RBRACK(yypos, yypos));
<INITIAL>. => (error("\n***Lexer errorbad character ***\n");
    raise Fail("Lexer error: bad character " ^yytext));

