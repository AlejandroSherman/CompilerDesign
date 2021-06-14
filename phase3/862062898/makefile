make:
				bison -v -d --file-prefix=y 862062898.y
				flex 862062898.lex
				g++ -std=c++11 -g -o my_compiler lex.yy.c y.tab.c -lfl


clean:
				rm -rf y.output y.tab.* my_compiler lex.yy.c input.txt *.mil
