parser: 862062898.lex
		    bison -v -d --file-prefix=y 862062898.y
		    flex 862062898.lex
		    gcc -o parser y.tab.c lex.yy.c -lfl

clean:
		    rm -f lex.yy.c y.tab.* y.output *.o parser
