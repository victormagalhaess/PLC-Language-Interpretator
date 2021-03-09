structure PlcFrontEnd = struct

  (* Create a structure for the PlcParser, by loading the lexer and parser definitions. *)
  structure PlcFELrVals = PlcParserLrValsFun(structure Token = LrParser.Token)
  structure PlcLexer = PlcLexerFun(structure Tokens = PlcFELrVals.Tokens);
  structure PlcParser =
        Join(structure LrParser = LrParser
              structure ParserData = PlcFELrVals.ParserData
              structure Lex = PlcLexer)

  (* Start the parsing given a stream of tokens *)
  val invoke = fn lexstream =>
    let
      fun printError (str, pos,_) =
        (TextIO.output(TextIO.stdOut, "\n***PLC Parser Error at line "  ^ PlcLexer.UserDeclarations.getLineAsString() ^ "***\n"); raise PlcParser.ParseError)
    in
      PlcParser.parse(0,lexstream, printError,())
    end

  (* Create a lexer given a function that reads an program input *)
  fun newLexer fcn =
    let
      val lexer = PlcParser.makeLexer fcn
      val _ = PlcLexer.UserDeclarations.init()
    in
      lexer
    end

  (* Create a lexer from a program input as string. *)
  fun stringToLexer str =
    let
      val done = ref false
    in
      newLexer(fn n => if (!done) then "" else (done := true;str))
    end

  (* Create a lexer reading a program from a file. *)
  fun fileToLexer filename =
    let
      val inStream = TextIO.openIn(filename)
    in
      newLexer(fn n => TextIO.inputAll(inStream))
    end

  (*Creates a parser from a lexer. *)
  fun lexerToParser(lexer) : expr =
    let
      val dummyEOF = PlcFELrVals.Tokens.EOF(0,0)
      val (result,lexer) = invoke lexer
      val (nextToken, lexer) = PlcParser.Stream.get lexer
    in
      if PlcParser.sameToken(nextToken, dummyEOF) then
        result
      else
        (TextIO.output(TextIO.stdOut, "*** PLC PARSER WARNING -- unconsumed input ***\n"); result)
    end


  val fromString = lexerToParser o stringToLexer
  val fromFile = lexerToParser o fileToLexer

end
