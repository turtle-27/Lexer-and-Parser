structure myCalcLrVals = myCalcLrValsFun(structure Token = LrParser.Token)
structure myCalcLex = myCalcLexFun(structure Tokens = myCalcLrVals.Tokens);
structure myCalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = myCalcLrVals.ParserData
     	       structure Lex = myCalcLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    myCalcParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  myCalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
fun takeInput infilename =
	let val infile = TextIO.openIn(infilename)
		val lexer = myCalcParser.makeLexer (fn _ => TextIO.inputAll(infile))
	in
		lexer
	end
fun parse (lexer) =
    let val dummyEOF = myCalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = myCalcParser.Stream.get lexer
    in
        if myCalcParser.sameToken(nextToken, dummyEOF) then result
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result);
	print("["^result^"]\n")
    end

val parseString = parse o takeInput



