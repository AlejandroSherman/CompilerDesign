%{
 #include <stdio.h>
 #include <stdlib.h>
 extern int currLine;
 extern int currPos;
 extern FILE * yyin;
 void yyerror(const char * msg) {
    printf("Error: On line %d, column %d: %s \n", currLine, currPos, msg);
 }
%}

%union{
    char * str;
    int    num;
}

%error-verbose
%start prog_start
%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO BEGINLOOP ENDLOOP BREAK READ WRITE TRUE FALSE RETURN SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET
%left ADD SUB
%left MULT DIV MOD
%left EQ NEQ LT GT LTE GTE
%left AND OR
%right NOT
%right ASSIGN
%token <num> NUMBER
%token <str> IDENT

%%

prog_start:	        functions
                    {printf("prog_start -> functions\n");}
                    | error {yyerrok; yyclearin;}
                    ;

functions: 	        function functions
      		          {printf("functions -> function functions\n");}
                    | /*nothing*/
                    {printf("functions -> epsilon\n");}
        		        ;

function:	          FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
          		      {printf("function -> FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY\n");}
                    ;

declarations:	      declaration SEMICOLON declarations
                    {printf("declarations -> declaration SEMICOLON declarations\n");}
                    | /*nothing*/
                    {printf("declarations -> epsilon\n");}
                    | declaration error {yyerrok;}
                  	;

declaration:	      identifiers COLON INTEGER
                    {printf("declaration -> identifiers COLON INTEGER\n");}
                    | identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
                    {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET OF INTEGER\n", $5);}
                    ;

identifiers:	      ident
                    {printf("identifiers -> ident\n");}
                    | ident COMMA identifiers
                    {printf("identifiers -> ident COMMA identifiers\n");}
                    ;

ident:		          IDENT
                 		{printf("ident -> IDENT %s\n", $1);}
                		;

statements:	        statement SEMICOLON
              		  {printf("statements -> statement SEMICOLON\n");}
                    |  statement SEMICOLON statements
                    {printf("statements -> statement SEMICOLON statements\n");}
                		| statement error {yyerrok;}
                		;

statement:	        go_var
              	  	{printf("statement -> go_var\n");}
              	  	| go_if
              		  {printf("statement -> go_if\n");}
                		| go_while
              		  {printf("statement -> go_while\n");}
                		| go_do
              		  {printf("statement -> go_for\n");}
                		| go_read
                    {printf("statement -> go_read\n");}
                		| go_write
              		  {printf("statement -> go_write\n");}
                		| go_break
              		  {printf("statement -> go_break\n");}
                		| go_return
              		  {printf("statement -> go_return\n");}
                		;

go_var:		          var ASSIGN expression
                		{printf("go_var -> var ASSIGN expression\n");}
                		;

go_if:		          IF bool_exp THEN statements ENDIF
                		{printf("go_if -> IF bool_exp THEN statements ENDIF\n");}
                		| IF bool_exp THEN statements ELSE statements ENDIF
                		{printf("go_if -> IF bool_exp THEN statements ELSE statements ENDIF\n");}
                		;

go_while:		        WHILE bool_exp BEGINLOOP statements ENDLOOP
                    {printf("go_while -> WHILE bool_expr BEGINLOOP statements ENDLOOP\n");}
                    ;

go_do:		          DO BEGINLOOP statements ENDLOOP WHILE bool_exp
                		{printf("go_do -> DO BEGINLOOP statements ENDLOOP WHILE bool_exp\n");}
                		;

vars:	              var
                    {printf("vars -> var\n");}
                    |  var COMMA vars
                    {printf("vars -> var COMMA vars\n");}
                    ;

go_read:		        READ vars
                    {printf("go_read -> READ vars\n");}
                    ;

go_write:		        WRITE vars
                    {printf("go_write -> WRITE vars\n");}
                    ;

go_break:	          BREAK
                	 	{printf("go_break -> BREAK\n");}
                		;

go_return:	        RETURN expression
                    {printf("go_return -> RETURN expression\n");}
                    ;

bool_exp:	          relation_and_exp
                	 	{printf("bool_exp -> relation_and_exp\n");}
                		| bool_exp OR relation_and_exp
              		  {printf("bool_exp -> bool_exp OR relation_and_exp\n");}
                		;

relation_and_exp:	  relation_exps
                  	{printf("relation_and_exp -> relation_exps\n");}
                    | relation_exps AND relation_exps
              		  {printf("relation_and_exp -> relation_exps AND relation_exps\n");}
                    ;

relation_exps:      relation_exp
                    {printf("relation_exps -> relation_exp\n");}
                    | NOT relation_exp
                    {printf("relation_exps -> NOT relation_exp\n");}
                    ;

relation_exp:	      expression comp expression
              		  {printf("relation_exp -> expression comp expression\n");}
                		| TRUE
              		  {printf("relation_exp -> TRUE\n");}
                		| FALSE
              		  {printf("relation_expr -> FALSE\n");}
                		| L_PAREN bool_exp R_PAREN
              		  {printf("relation_expr -> L_PAREN bool_exp R_PAREN\n");}
                		;

comp:		            EQ
                    {printf("comp -> EQ\n");}
                    | NEQ
                    {printf("comp -> NEQ\n");}
                    | LT
                    {printf("comp -> LT\n");}
                    | GT
                    {printf("comp -> GT\n");}
                    | LTE
                    {printf("comp -> LTE\n");}
                    | GTE
                    {printf("comp -> GTE\n");}
                    ;

expression:	        multiplicative_expression ADD multiplicative_expression
                    {printf("expression -> multiplicative_expression ADD multiplicative_expression\n");}
                    | multiplicative_expression SUB multiplicative_expression
                    {printf("expression -> multiplicative_expression SUB multiplicative_expression\n");}
                    | multiplicative_expression
                    {printf("expression -> multiplicative_expression\n");}
                    | error {yyerrok;}
                    ;

multiplicative_expression:     term MULT term
                    {printf("multiplicative_expression -> term MULT term\n");}
                    | term DIV term
                    {printf("multiplicative_expression -> term DIV term\n");}
                    | term MOD term
                    {printf("multiplicative_expression -> term MOD term\n");}
                    | term
                    {printf("multiplicative_expression -> term\n");}
                    ;

term:		            var
                    {printf("term -> var\n");}
                    | SUB var
                    {printf("term -> SUB var\n");}
                    | NUMBER
                    {printf("term -> NUMBER %d\n", $1);}
                    | SUB NUMBER
                    {printf("term -> SUB NUMBER %d\n", $2);}
                    | L_PAREN expression R_PAREN
                    {printf("term -> L_PAREN expression R_PAREN\n");}
                    | SUB L_PAREN expression R_PAREN
                    {printf("term -> SUB L_PAREN expression R_PAREN\n");}
                    | ident L_PAREN expressions R_PAREN
                    {printf("term -> ident L_PAREN expressions R_PAREN\n");}
                    ;

expressions:	      expression
                    {printf("expressions -> expression\n");}
                    | expression COMMA expressions
                    {printf("expressions -> expression COMMA expressions\n");}
                    | /*epsilon*/
                    {printf("expressions -> epsilon\n");}
                    ;

var:		            ident
                    {printf("var -> ident\n");}
                		| ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET
              		  {printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
                		;

%%


int main(int argc, char ** argv) {
	if (argc >= 2) {
		yyin = fopen(argv[1], "r");
		if (yyin == NULL) {
			yyin = stdin;
		}
	}
	else {
		yyin = stdin;
	}
	yyparse();
	return 1;
}
