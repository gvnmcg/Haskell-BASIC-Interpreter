all: clean basic

clean: clBasic clBasicData clBasicInterpreter clBasicParser clParselib clOutput

clBasic: 
	rm -f BASIC BASIC.hi BASIC.o

clBasicData:
	rm -f BasicData.hi BasicData.o 

clBasicInterpreter:
	rm -f BasicInterpreter.hi BasicInterpreter.o Util.hi Util.o Variables.hi Variables.o

clBasicParser:
	rm -f BasicParser.hi BasicParser.o

clParselib:
	rm -f Parselib.hi Parselib.o

clOutput:
	rm -f output

basic:
	ghc BASIC.hs

run: clOutput
	./run.sh > output