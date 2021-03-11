signature PlcParser_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val FUN:  'a * 'a -> (svalue,'a) token
val CINT: (int) *  'a * 'a -> (svalue,'a) token
val NAME: (string) *  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LKEY:  'a * 'a -> (svalue,'a) token
val RKEY:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val NIL:  'a * 'a -> (svalue,'a) token
val FUNARROW:  'a * 'a -> (svalue,'a) token
val UNDERSCORE:  'a * 'a -> (svalue,'a) token
val VERTBAR:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SEMIC:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val DOUBLEPTS:  'a * 'a -> (svalue,'a) token
val SMALLEREQ:  'a * 'a -> (svalue,'a) token
val SMALLER:  'a * 'a -> (svalue,'a) token
val DIF:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MULTI:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val ISEMPTY:  'a * 'a -> (svalue,'a) token
val TAIL:  'a * 'a -> (svalue,'a) token
val HEAD:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val NEGATION:  'a * 'a -> (svalue,'a) token
val WITH:  'a * 'a -> (svalue,'a) token
val MATCH:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val RECURSIVE:  'a * 'a -> (svalue,'a) token
val FN:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
end
signature PlcParser_LRVALS=
sig
structure Tokens : PlcParser_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
