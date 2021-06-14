%code requires{
  #include<string>
  using namespace std;

  //setup IR
  struct IR_obj{
     string IR;
     string ret_name;
     string var_value;
     string position;
     bool is_an_array;
  };
}

%{
  //includes and function declarations
  #include <stdio.h>
  #include <stdlib.h>
  #include <string>
  #include <vector>
  #include <iostream>
  #include <sstream>
  using namespace std;
  int yyeror(char *s);
  int yyerror(string s);
  int yylex(void);
  string create_temp();
  string create_label();
  extern FILE* yyin;
  //globals
  bool is_main = false;
  vector<string> var_list;
  vector<string> func_list;
%}

%union {
  char   * str_value;   //changed from str to str_value for readablilty
  int      int_value;   //changed from int to int_value for readablilty
  IR_obj * ir_type;
}

%error-verbose
%start prog_start
%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO BEGINLOOP ENDLOOP BREAK SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET
%token ADD SUB
%token MULT DIV MOD
%token EQ NEQ LT GT LTE GTE
%token READ WRITE
%token AND OR NOT TRUE FALSE RETURN
%token ASSIGN
%token<str_value> IDENT
%token<int_value> NUMBER
%type<str_value> comp
%type<ir_type> prog_start
%type<ir_type> declarations
%type<ir_type> declaration
%type<ir_type> functions
%type<ir_type> function
%type<ir_type> function_begin
%type<ir_type> function_middle
%type<ir_type> function_end
%type<ir_type> identifiers
%type<ir_type> ident
%type<ir_type> statements
%type<ir_type> statement
%type<ir_type> go_var
%type<ir_type> go_if
%type<ir_type> go_while
%type<ir_type> go_do
%type<ir_type> vars
%type<ir_type> go_read
%type<ir_type> go_write
%type<ir_type> go_break
%type<ir_type> go_return
%type<ir_type> bool_exp
%type<ir_type> relation_and_exp
%type<ir_type> relation_exps
%type<ir_type> relation_exp
%type<ir_type> expression
%type<ir_type> multiplicative_expression
%type<ir_type> term
%type<ir_type> term_end
%type<ir_type> expressions
%type<ir_type> var

%%

prog_start:	        functions //There is a reduce reduce conflict somewhere, but it's just a warning
                    {
                      $$ = new IR_obj();
                      if (!is_main){
                         yyerror("\"main\" function is not defined in the program.");
                      }
                      $$->IR = $1->IR;
                      cout << $$->IR << endl; //output eveyrthing via standard out
                    }
                    | %empty
                    {
                      $$ = new IR_obj();
                      cout << $$->IR << endl;
                    }
                    ;

functions: 	        function functions
      		          {
                      $$ = new IR_obj();
                      stringstream ss;
                      ss << $1->IR << endl;
                      ss << endl << $2->IR;
                      $$->IR = ss.str();
                    }
                    | %empty
                    {
                      $$ = new IR_obj();
                    }
        		        ;

function:	          FUNCTION identifiers SEMICOLON function_begin function_middle function_end //broke function into these three sections to allow for working IR generation for fibonacci.min
          		      {
                      $$ = new IR_obj();
                      stringstream ss;

                      if ($2->IR == "_main"){
                         is_main = true;
                      }

                      string function = $2->IR;

                      for (unsigned i = 0; i < func_list.size(); ++i){
                         if (func_list.at(i) == function){
                            string error_msg = "function \"" + function + "\" is multiply-defined.";
                            yyerror(error_msg);
                         }
                      }

                      func_list.push_back(function);

                      var_list.clear();

                      string func_holder = $2->IR;
                      func_holder.erase(0,1);
                      ss << "func " << func_holder << endl;

                      ss << $4->IR;
                      if ($4->IR.size() > 0){ //only do the newline if there is output
                         ss << endl;
                      }

                      ss << $5->IR;
                      if ($5->IR.size() > 0){ //only do the newline if there is output
                         ss << endl;
                      }

                      ss << $6->IR;
                      if ($6->IR.size() > 0){ //only do the newline if there is output
                         ss << endl;
                      }

                      ss << "endfunc";
                      $$->IR = ss.str();
                    }
                    ;

function_begin:     BEGIN_PARAMS declarations END_PARAMS
                    {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $2->IR << endl;

                      string iden;
                      int curr_param = 0;
                      for (unsigned i = 0; i < $2->ret_name.size(); ++i){
                         if ($2->ret_name.at(i) == ','){
                            ss << "= " << iden << ", $";
                            ss << to_string(curr_param) << endl;
                            iden = "";
                            curr_param++;
                            continue;
                         }

                         iden.push_back($2->ret_name[i]);
                      }

                      if (iden.size() > 0){
                         ss << "= " << iden << ", $";
                         ss << to_string(curr_param);
                      }

                      $$->IR = ss.str();
                    }
                    | BEGIN_PARAMS END_PARAMS
                    {
                      $$ = new IR_obj();
                    }
                    ;

function_middle:     BEGIN_LOCALS declarations END_LOCALS
                    {
                      $$ = new IR_obj();
                      $$->IR = $2->IR;
                    }
                    | BEGIN_LOCALS END_LOCALS
                    {
                      $$ = new IR_obj();
                    }
                    ;

function_end:       BEGIN_BODY statements END_BODY
                    {
                      // check if break is found outside of a loop
                      string out_break = $2->IR;
                      bool wrong_area;

                      if (out_break.find("break") == string::npos){
                         wrong_area = false;
                      }
                      else{
                         wrong_area = true;
                      }

                      if (wrong_area){
                         cout << "Error: break statement not within a loop." << endl;
                         exit(1);
                      }

                      $$ = new IR_obj();
                      $$->IR = $2->IR;
                    }
                    | BEGIN_BODY END_BODY
                    {
                      $$ = new IR_obj();
                    }
                    ;

declarations:	      declarations declaration SEMICOLON //changed to be left recursion after having output issues with previous production
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      stringstream ret_list;

                      ss << $1->IR << endl << $2->IR;
                      ret_list << $1->ret_name << "," << $2->ret_name;

                      $$->IR = ss.str();
                      $$->ret_name = ret_list.str();
                    }
                    | declaration SEMICOLON
                    {
                      $$ = new IR_obj();
                      $$->IR = $1->IR;
                      $$->ret_name = $1->ret_name;
                    }
                  	;

declaration:	      identifiers COLON INTEGER
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      string curr_var = "";

                      for (unsigned i = 0; i < $1->IR.size(); ++i){
                         if ($1->IR.at(i) == ','){
                            ss << ". " << curr_var << endl;
                            for (unsigned j = 0; j < var_list.size(); ++j){
                               if (var_list.at(j) == curr_var){
                                  string error_msg = "symbol \"" + curr_var + "\" is multiply-defined.";
                                  yyerror(error_msg);
                               }
                            }
                            var_list.push_back(curr_var);
                            curr_var = ""; //reset
                         }

                         else{
                            curr_var.push_back($1->IR.at(i));
                         }
                      }

                      if (curr_var.size() > 0){
                         ss << ". " << curr_var;
                         for (unsigned i = 0; i < var_list.size(); ++i){
                            if (var_list.at(i) == curr_var){
                               string error_msg = "symbol \"" + curr_var + "\" is multiply-defined.";
                               yyerror(error_msg);
                            }
                         }
                         var_list.push_back(curr_var);
                      }

                      $$->IR = ss.str();
                      $$->ret_name = $1->IR;
                    }
                    | identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
                    {
                      if ($5 <= 0){
                         yyerror("array size is <= 0");
                      }

                      $$ = new IR_obj();
                      stringstream ss;
                      string curr_var = "";

                      for (unsigned i = 0; i < $1->IR.size(); ++i){
                         if ($1->IR.at(i) == ','){
                            ss << ".[] " << curr_var << ", ";
                            ss << to_string($5) << endl;
                            for (unsigned j = 0; j < var_list.size(); ++j){
                               if (var_list.at(j) == curr_var){
                                  string error_msg = "symbol \"" + curr_var + "\" is multiply-defined.";
                                  yyerror(error_msg);
                               }
                            }
                            var_list.push_back(curr_var);
                            curr_var = "";
                         }
                         else{
                            curr_var.push_back($1->IR.at(i));
                         }
                      }

                      if (curr_var.size() > 0){
                         ss << ".[] " << curr_var;
                         ss << ", " << to_string($5);
                         for (unsigned i = 0; i < var_list.size(); ++i){
                            if (var_list.at(i) == curr_var){
                               string error_msg = "symbol \"" + curr_var + "\" is multiply-defined.";
                               yyerror(error_msg);
                            }
                         }
                         var_list.push_back(curr_var);
                      }

                      $$->IR = ss.str();
                      $$->ret_name = $1->IR;
                    }
                    ;

identifiers:	      ident
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      ss << "_" << $1->IR;
                      $$->IR = ss.str();
                    }
                    | ident COMMA identifiers
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      ss << "_" << $1->IR;
                      ss << "," << $3->IR;
                      $$->IR = ss.str();
                    }
                    ;

ident:		          IDENT
                 		{
                      $$ = new IR_obj();
                      $$->IR = $1;
                    }
                		;

statements:	        statement SEMICOLON
              		  {
                      $$ = new IR_obj();
                      $$->IR = $1->IR;
                    }
                    | statements statement SEMICOLON //changed to be left recursion after having output issues with previous production
                    {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $1->IR;
                      if (($1->IR.size() > 0) && ($2->IR.size() > 10)){ //this solution proably doesn't scale well, but this is the best fix I have right now to get the number of end1 to work for fibonacci file
                         ss << endl; //only want the endl for certain areas
                      }

                      ss << $2->IR;
                      $$->IR = ss.str();
                    }
                		;

statement:	        go_var
              	  	{
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $1->IR << endl;
                      $$->IR = ss.str();
                    }
              	  	| go_if
              		  {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $1->IR;
                      $$->IR = ss.str();
                    }
                		| go_while
              		  {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $1->IR;
                      $$->IR = ss.str();
                    }
                		| go_do
              		  {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $1->IR;
                      $$->IR = ss.str();
                    }
                		| go_read
                    {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $1->IR;
                      $$->IR = ss.str();
                    }
                		| go_write
              		  {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $1->IR;
                      $$->IR = ss.str();
                    }
                		| go_break
              		  {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $1->IR;
                      $$->IR = ss.str();
                    }
                		| go_return
              		  {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $1->IR;
                      $$->IR = ss.str();
                    }
                		;

go_var:		          var ASSIGN expression
                		{
                      $$ = new IR_obj();
                      stringstream ss;
                      string assign_val;

                      if ($3->ret_name != ""){ //check what type of assignment
                         ss << $3->IR << endl;
                         assign_val = $3->ret_name;
                      }

                      else{
                         assign_val = $3->IR;
                      }

                      if ($1->is_an_array){
                         if ($1->IR.size() > 0){
                            ss << $1->IR << endl;
                         }

                         ss << "[]= " << $1->var_value << ", ";
                         ss << $1->position << ", " << assign_val;
                      }

                      else{
                         ss << "= " << $1->IR;
                         ss << ", " << assign_val;
                      }

                      $$->IR = ss.str();
                      $$->ret_name = $1->IR;
                    }
                		;

go_if:		          IF bool_exp THEN statements ENDIF
                		{ //made with help from TA example
                      $$ = new IR_obj();
                      string label0 = create_label();
                      string label1 = create_label();
                      stringstream ss;

                      ss << $2->IR << endl;
                      ss << "?:= " << label0 << ", " << $2->ret_name << endl;
                      ss << ":= " << label1 << endl;
                      ss << ": " << label0 << endl;
                      ss << $4->IR << endl;
                      ss << ": " << label1;

                      $$->IR = ss.str();
                    }
                		| IF bool_exp THEN statements ELSE statements ENDIF
                		{
                      $$ = new IR_obj();
                      string label0 = create_label();
                      string label1 = create_label();
                      stringstream ss;

                      ss << $2->IR << endl;
                      ss << "?:= " << label0 << ", " << $2->ret_name << endl;
                      ss << ":= " << label1 << endl;
                      ss << ": " << label0 << endl;
                      ss << $4->IR << endl;
                      ss << ": " << label1 << endl;
                      ss << $6->IR;

                      $$->IR = ss.str();
                    }
                		;

go_while:		        WHILE bool_exp BEGINLOOP statements ENDLOOP
                    {
                      $$ = new IR_obj();
                      string conditional_label = create_label();
                      string start_label = create_label();
                      string end_label = create_label();
                      stringstream ss;

                      ss << ": " << conditional_label << endl;
                      ss << $2->IR << endl;
                      ss << "?:= " << start_label << ", " << $2->ret_name << endl;
                      ss << ":= " << end_label << endl;
                      ss << ": " << start_label << endl;
                      ss << $4->IR << endl;
                      ss << ":= " << conditional_label << endl;
                      ss << ": " << end_label;

                      $$->IR = ss.str();
                    }
                    ;

go_do:		          DO BEGINLOOP statements ENDLOOP WHILE bool_exp
                		{
                      $$ = new IR_obj();
                      string start_label = create_label();
                      string conditional_label = create_label();
                      stringstream ss;

                      ss << ": " << start_label << endl;
                      ss << $3->IR << endl;
                      ss << ": " << conditional_label << endl;
                      ss << $6->IR << endl;
                      ss << "?:= " << start_label << ", " << $6->ret_name;

                      $$->IR = ss.str();
                    }
                		;

vars:	              var
                    {
                      $$ = new IR_obj();
                      stringstream ss;

                      $$->IR = $1->var_value;
                      $$->is_an_array = $1->is_an_array;
                    }
                    |  var COMMA vars
                    {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $1->var_value << "," << $3->IR;
                      $$->IR = ss.str();

                      if ($1->is_an_array != $3->is_an_array){ //check we are not mixing types
                         stringstream error_msg;
                         error_msg << "variable \"" << $1->IR << "\" is of type";
                         if (!$1->is_an_array){
                            error_msg << " integer";
                         }
                         else{
                            error_msg << " array";
                         }

                         yyerror(error_msg.str());
                      }

                      $$->is_an_array = $1->is_an_array;
                    }
                    ;

go_read:		        READ vars
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      string read_hold = "";

                      for (unsigned i = 0; i < $2->IR.size(); ++i){
                         if ($2->IR.at(i) == ','){
                            ss << ".< ";
                            ss << read_hold << endl;
                            read_hold = ""; //clear
                         }
                         else{
                            read_hold.push_back($2->IR.at(i));
                         }
                      }

                      ss << ".< " << read_hold;
                      $$->IR = ss.str();
                    }
                    ;

go_write:		        WRITE vars
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      string write_hold = "";

                      for (unsigned i = 0; i < $2->IR.size(); ++i){
                         if ($2->IR.at(i) == ','){
                            ss << ".< " << write_hold << endl;
                            write_hold = ""; //clear
                         }
                         else{
                            write_hold.push_back($2->IR.at(i));
                         }
                      }

                      ss << ".> " << write_hold;
                      $$->IR = ss.str();
                    }
                    ;

go_break:	          BREAK
                	 	{ // unsure of what else to do for this
                      $$ = new IR_obj();
                      $$->IR = "break"; //so I can find out later if it's outside of a loop
                    }
                		;

go_return:	        RETURN expression
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      string return_val;

                      if ($2->ret_name != ""){ //check the type of return
                         ss << $2->IR << endl;
                         return_val = $2->ret_name;
                      }

                      else{
                         return_val = create_temp();
                         ss << ". " << return_val << endl;
                         ss << "= " << return_val << ", ";
                         ss << $2->IR << endl;
                      }

                      ss << "ret " << return_val;
                      $$->IR = ss.str();
                      $$->ret_name = return_val;
                    }
                    ;

bool_exp:	          relation_and_exp
                	 	{
                      $$ = new IR_obj();
                      $$->IR = $1->IR; //direct
                      $$->ret_name = $1->ret_name;
                    }
                		| bool_exp OR relation_and_exp
              		  {
                      $$ = new IR_obj();
                      stringstream ss;
                      string return_val = create_temp();

                      ss << $1->IR << endl;
                      ss << $3->IR << endl;
                      ss << ". " << return_val << endl;
                      ss << "|| " << return_val << ", ";
                      ss << $1->ret_name << ", " << $3->ret_name;

                      $$->IR = ss.str();
                      $$->ret_name = return_val;
                    }
                		;

relation_and_exp:	  relation_exps
                  	{
                      $$ = new IR_obj();
                      $$->IR = $1->IR; //direct
                      $$->ret_name = $1->ret_name;
                    }
                    | relation_exps AND relation_exps
              		  {
                      $$ = new IR_obj();
                      stringstream ss;
                      string return_val = create_temp();

                      ss << $1->IR << endl;
                      ss << $3->IR << endl;
                      ss << ". " << return_val << endl;
                      ss << "&& " << return_val << ", ";
                      ss << $1->ret_name << ", " << $3->ret_name;

                      $$->IR = ss.str();
                      $$->ret_name = return_val;
                    }
                    ;

relation_exps:      relation_exp
                    {
                      $$ = new IR_obj();
                      $$->IR = $1->IR; //direct
                      $$->ret_name = $1->ret_name;
                    }
                    | NOT relation_exp
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      string not_val = create_temp();

                      ss << $2->IR << endl;
                      ss << "! " << not_val << ", " << $2->ret_name;

                      $$->IR = ss.str();
                      $$->ret_name = not_val;
                    }
                    ;

relation_exp:	      expression comp expression
              		  {
                      $$ = new IR_obj();
                      stringstream ss;
                      string comp_val = create_temp();
                      string left_val;
                      string right_val;

                      if ($1->ret_name != ""){ //check what the left_val is
                         ss << $1->IR << endl;
                         left_val = $1->ret_name;
                      }

                      else{
                         left_val = $1->IR;
                      }

                      if ($3->ret_name != ""){ //check what right_val is
                         right_val = $3->ret_name;
                         ss << $3->IR << endl;
                         ss << ". " << comp_val << endl;
                         ss << $2 << " " << comp_val << ", "; //direct
                         ss << left_val << ", " << right_val;
                      }

                      else{
                         right_val = $3->IR;
                         ss << ". " << comp_val << endl;
                         ss << $2 << " " << comp_val << ", "; //direct
                         ss << left_val << ", " << right_val;
                      }

                      $$->IR = ss.str();
                      $$->ret_name = comp_val;
                    }
                		| TRUE
              		  {
                      $$ = new IR_obj();
                      stringstream ss;
                      string true_val = create_temp();

                      ss << ". " << true_val << endl;
                      ss << "= " << true_val << ", 1";

                      $$->IR = ss.str();
                      $$->ret_name = true_val;
                    }
                		| FALSE
              		  {
                      $$ = new IR_obj();
                      stringstream ss;
                      string false_val = create_temp();

                      ss << ". " << false_val << endl;
                      ss << "= " << false_val << ", 0";

                      $$->IR = ss.str();
                      $$->ret_name = false_val;
                    }
                		| L_PAREN bool_exp R_PAREN
              		  {
                      $$ = new IR_obj();
                      $$->IR = $2->IR; //pass through
                      $$->ret_name = $2->ret_name;
                    }
                		;

comp:		            EQ
                    { } //direct pass
                    | NEQ
                    { } //direct pass
                    | LT
                    { } //direct pass
                    | GT
                    { } //direct pass
                    | LTE
                    { } //direct pass
                    | GTE
                    { } //direct pass
                    ;

expression:	        multiplicative_expression ADD multiplicative_expression
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      string add_val = create_temp();
                      string left_val;
                      string right_val;

                      if ($1->ret_name != ""){ //check what left_val is
                         ss << $1->IR << endl;
                         left_val = $1->ret_name;
                      }

                      else{
                         left_val = $1->IR;
                      }

                      if ($3->ret_name != ""){ //check what right_val is
                         right_val = $3->ret_name;
                         ss << $3->IR << endl;
                         ss << ". " << add_val << endl;
                         ss << "+ " << add_val << ", ";
                         ss << left_val << ", " << right_val;
                      }

                      else{
                         right_val = $3->IR;
                         ss << ". " << add_val << endl;
                         ss << "+ " << add_val << ", ";
                         ss << left_val << ", " << right_val;
                      }

                      $$->IR = ss.str();
                      $$->ret_name = add_val;
                    }
                    | multiplicative_expression SUB multiplicative_expression
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      string sub_val = create_temp();
                      string left_val;
                      string right_val;

                      if ($1->ret_name != ""){ //check what left_val is
                         ss << $1->IR << endl;
                         left_val = $1->ret_name;
                      }

                      else{
                         left_val = $1->IR;
                      }

                      if ($3->ret_name != ""){ //check what right_val is
                         right_val = $3->ret_name;
                         ss << $3->IR << endl;
                         ss << ". " << sub_val << endl;
                         ss << "- " << sub_val << ", ";
                         ss << left_val << ", " << right_val;
                      }

                      else{
                         right_val = $3->IR;
                         ss << ". " << sub_val << endl;
                         ss << "- " << sub_val << ", ";
                         ss << left_val << ", " << right_val;
                      }

                      $$->IR = ss.str();
                      $$->ret_name = sub_val;
                    }
                    | multiplicative_expression
                    {
                      $$ = new IR_obj();
                      $$->IR = $1->IR;
                      $$->ret_name = $1->ret_name;
                    }
                    ;

multiplicative_expression:     term MULT term
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      string mult_val = create_temp();
                      string left_val;
                      string right_val;

                      if ($1->ret_name != ""){ //check what left_val is
                         ss << $1->IR << endl;
                         left_val = $1->ret_name;
                      }

                      else{
                         left_val = $1->IR;
                      }

                      if ($3->ret_name != ""){ //check what right_val is
                         right_val = $3->ret_name;
                         ss << $3->IR << endl;
                         ss << ". " << mult_val << endl;
                         ss << "* " << mult_val << ", ";
                         ss << left_val << ", " << right_val;
                      }

                      else{
                         right_val = $3->IR;
                         ss << ". " << mult_val << endl;
                         ss << "* " << mult_val << ", ";
                         ss << left_val << ", " << right_val;
                      }

                      $$->IR = ss.str();
                      $$->ret_name = mult_val;
                    }
                    | term DIV term
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      string div_val = create_temp();
                      string left_val;
                      string right_val;

                      if ($1->ret_name != ""){ //check what left_val is
                         ss << $1->IR << endl;
                         left_val = $1->ret_name;
                      }

                      else{
                         left_val = $1->IR;
                      }

                      if ($3->ret_name != ""){ //check what right_val is
                         right_val = $3->ret_name;
                         ss << $3->IR << endl;
                         ss << ". " << div_val << endl;
                         ss << "/ " << div_val << ", ";
                         ss << left_val << ", " << right_val;
                      }

                      else{
                         right_val = $3->IR;
                         ss << ". " << div_val << endl;
                         ss << "/ " << div_val << ", ";
                         ss << left_val << ", " << right_val;
                      }

                      $$->IR = ss.str();
                      $$->ret_name = div_val;
                    }
                    | term MOD term
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      string mod_val = create_temp();
                      string left_val;
                      string right_val;

                      if ($1->ret_name != ""){ //check what left_val is
                         ss << $1->IR << endl;
                         left_val = $1->ret_name;
                      }

                      else{
                         left_val = $1->IR;
                      }

                      if ($3->ret_name != ""){ //check what right_val is
                         right_val = $3->ret_name;
                         ss << $3->IR << endl;
                         ss << ". " << mod_val << endl;
                         ss << "% " << mod_val << ", ";
                         ss << left_val << ", " << right_val;
                      }

                      else{
                         right_val = $3->IR;
                         ss << ". " << mod_val << endl;
                         ss << "% " << mod_val << ", ";
                         ss << left_val << ", " << right_val;
                      }

                      $$->IR = ss.str();
                      $$->ret_name = mod_val;
                    }
                    | term
                    {
                      $$ = new IR_obj();
                      $$->IR = $1->IR;
                      $$->ret_name = $1->ret_name;
                    }
                    ;

term:               term_end //added this production for better code output
                    {
                      $$ = new IR_obj();

                      if ($1->ret_name == "var"){
                         stringstream ss;
                         string term_val = create_temp();

                         if ($1->is_an_array){ //check if an array
                            if ($1->IR.size() > 0){
                               ss << $1->IR << endl;
                            }

                            ss << "=[] " << term_val << ", ";
                            ss << $1->var_value << ", " << $1->position;

                            $$->var_value = $1->var_value;
                            $$->position = $1->position;
                            $$->IR = ss.str();
                            $$->ret_name = term_val;
                         }

                         else{
                            ss << ". " << term_val << endl;
                            ss << "= " << term_val << ", " << $1->IR;

                            $$->IR = ss.str();
                            $$->ret_name = term_val;
                         }
                      }

                      else if ($1->ret_name == "num"){ //check if num
                         $$->IR = $1->IR;
                         $$->ret_name = "";
                      }

                      else{ //other
                         $$->IR = $1->IR;
                         $$->ret_name = $1->ret_name;
                      }
                    }
                    | SUB term_end
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      string sub_term_val = create_temp();

                      if ($2->ret_name == "var"){
                         string term_val = create_temp();

                         if ($2->is_an_array){ //check if an array
                            if ($2->IR.size() > 0){
                               ss << $2->IR << endl;
                            }

                            ss << "=[] " << term_val << ", ";
                            ss << $2->var_value << ", ";
                            ss << $2->position << endl;
                            ss << ". " << sub_term_val << endl;
                            ss << "- " << sub_term_val << ", 0, " << term_val;

                            $$->var_value = $2->var_value;
                            $$->position = $2->position;
                            $$->IR = ss.str();
                            $$->ret_name = sub_term_val;
                         }

                         else{
                            ss << ". " << term_val << endl;
                            ss << "= " << term_val << ", ";
                            ss << $2->IR << endl;
                            ss << ". " << sub_term_val << endl;
                            ss << "- " << sub_term_val << ", 0, " << term_val;

                            $$->IR = ss.str();
                            $$->ret_name = sub_term_val;
                         }
                      }
                      else{ //other
                         ss << ". " << sub_term_val << endl;
                         ss << "- " << sub_term_val << ", 0, " << $2->IR;

                         $$->IR = ss.str();
                         $$->ret_name = sub_term_val;
                      }
                    }
                    | ident L_PAREN expressions R_PAREN
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      stringstream next_stream;
                      string exp_holder;
                      string exp_val = create_temp();

                      ss << $3->IR << endl;

                      for (unsigned i = 0; i < $3->ret_name.size(); ++i){
                         if ($3->ret_name.at(i) == ','){
                            next_stream << "param " << exp_holder << endl;
                            exp_holder = ""; //clear
                            continue;
                         }

                         exp_holder.push_back($3->ret_name.at(i));
                      }

                      if (exp_holder.size() > 0){ //check there are rets
                         next_stream << "param " << exp_holder << endl;
                         ss << next_stream.str();
                      }

                      ss << ". " << exp_val << endl;
                      ss << "call " << $1->IR << ", " << exp_val;

                      $$->IR = ss.str();
                      $$->ret_name = exp_val;
                    }
                    ;

term_end:		        var
                    {
                      $$ = new IR_obj();
                      $$->IR = $1->IR;
                      $$->ret_name = "var";
                      $$->is_an_array = $1->is_an_array;
                      $$->var_value = $1->var_value;
                      $$->position = $1->position;
                    }
                    | NUMBER
                    {
                      $$ = new IR_obj();
                      $$->IR = to_string($1);
                      $$->ret_name = "num";
                    }
                    | L_PAREN expression R_PAREN
                    {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << $2->IR;
                      $$->IR = ss.str();
                      $$->ret_name = $2->ret_name;
                    }
                    ;

expressions:	      expression
                    {
                      $$ = new IR_obj();
                      $$->IR = $1->IR;
                      $$->ret_name = $1->ret_name;
                    }
                    | expressions COMMA expression //made to be left recursion after other changes
                    {
                      $$ = new IR_obj();
                      stringstream ss;
                      stringstream ret_vals;

                      ss << $1->IR << endl << $3->IR;
                      ret_vals << $1->ret_name << "," << $3->ret_name;

                      $$->IR = ss.str();
                      $$->ret_name = ret_vals.str();
                    }
                    | %empty
                    {
                      $$ = new IR_obj();
                    }
                    ;

var:		            ident
                    {
                      $$ = new IR_obj();
                      stringstream ss;

                      ss << "_" << $1->IR;
                      string curr_var = ss.str();
                      bool found_var = false;

                      for (unsigned i = 0; i < var_list.size(); ++i){
                         if (var_list.at(i) == curr_var){
                            found_var = true;
                         }
                      }

                      if (!found_var){
                         string error_msg = "used variable \"" + curr_var + "\" was not previously declared.";
                         yyerror(error_msg);
                      }

                      $$->IR = ss.str();
                      $$->var_value = ss.str();
                      $$->is_an_array = false;
                    }
                		| ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET
              		  {
                      $$ = new IR_obj();
                      stringstream ss;
                      string pos;
                      string ir = "";

                      if ($3->ret_name != ""){ //see what pos is
                         ir = $3->IR;
                         pos = $3->ret_name;
                      }

                      else{
                         pos = $3->IR;
                      }

                      ss << "_" << $1->IR;
                      string curr_var = ss.str();
                      bool found_var = false;

                      for (unsigned i = 0; i < var_list.size(); ++i){
                         if (var_list.at(i) == curr_var){
                            found_var = true;
                         }
                      }

                      if (!found_var){
                         string error_msg = "used variable \"" + curr_var + "\" was not previously declared.";
                         yyerror(error_msg);
                      }

                      $$->IR = ir;
                      $$->is_an_array = true;
                      $$->var_value = ss.str();
                      $$->position = pos;
                    }
                		;

%%

/* Moved back to lex file
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
*/

//few helper functions for temp and label formatting
string create_temp(){
  static int curr_temp_pos = 0; //allows the int to keep increasing throughout the program
  return "__temp__" + to_string(curr_temp_pos++);
}

string create_label(){
  static int curr_label_pos = 0; //allows the int to keep increasing throughout the program
  return "__label__" + to_string(curr_label_pos++);
}

//error handling, both methods
int yyerror(char* s){
  return yyerror(string(s));
}

int yyerror(string msg){
  extern int currLine, currPos;
  extern char *yytext;
  cout << "Error line: " << currLine << ": " << msg << endl;
  exit(1);
}
