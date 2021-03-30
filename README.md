# PLC Language Interpreter

This repo contains all files necessary to run the PLC language interpreter. <br>
The PLC language is a purely functional, static, strictly typed and higher-order based. It is based and developed over the Standard ML of New Jersey.
The description of the language, it's syntax, parse and lexer characteristics are described in the "project.pdf" currently in portuguese only. <br>

## Usage

To run the PLC Language Interpreter you should run the following commands inside the project folder:

```sh
ml-yacc PlcParser.yacc
ml-lex PlcParser.lex
sml Plc.sml
```

After loading all files, the interative terminal should accept PLC language commands using the following syntax:

```ml
val e = fromString "MY PLC CODE";
run e;
```

Otherwise you can also load PLC code from a file using the following syntax:

```ml
val e = fromFile "pathToMyFile.plc";
run e;
```

## Test

A few tests are contained within the test files, beeing them:

- testChecker.sml,
- testEnviron.sml,
- testInterp.sml,
- testParser.sml and
- testAll.sml

You can run each file individually or run all tests running the testAll.sml file.
