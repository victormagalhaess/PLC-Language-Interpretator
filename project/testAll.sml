exception codeError;

(*Testing Parser => Pass all the testParserCases through a function and validate if the parser is working*)
use "testParser.sml";

(*Testing PlcChecker Exceptions => Pass PLC code snippets that SHOULD throw exceptions from the PlcChecker through
teval to check if the exception throwing of PlcChecker is actually working *)
use "testChecker.sml";

(*Testing PlcInterp Exceptions => Pass PLC code snippets that SHOULD throw exceptions from the PlcInterp through
eval to check if the exception throwing of PlcInterp is actually working *)
use "testInterp.sml";

(*Testing Environ Exceptions => Pass PLC code snippets that SHOULD throw exceptions from the PlcEnviron through eval 
to check if the exception throwing of Eviron is actually working *)
use "testEnviron.sml";

(*Testing the PLC interpreter exceptions => Pass PLC code snippets that SHOULD NOT throw exceptions 
(extracted from the testParserCases) through eval to check if the PLC interpreter is not throwing unwanted exceptions *)
use "testParserCases.sml";
fun runAllTestParserCasesThroughCheckerAndInterpreter ([]) = "SUCCESS\n"
    | runAllTestParserCasesThroughCheckerAndInterpreter ((x:string,y:expr)::t) = 
        let in 
            val2string(eval(fromString(x))[]);
            runAllTestParserCasesThroughCheckerAndInterpreter(t) 
        end; 

(*Testing PLC code execution => Pass the PLC code snippet from example.plc through the interpreter and evaluate its
result checking if it is working. If the result is "15" then it works. *)
use "Plc.sml";
val result = val2string (eval(fromFile ("example.plc"))[]);
if result <> "15" then raise codeError else true;

(*Finally, if none of the last six verifications failed, then all the tests are OK and the PLC Interpreter should be
working accordingly to it's specifications *)
print ("If no exception or error were raised then all six tests were successful!\n");
