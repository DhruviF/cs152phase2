    /* cs152-miniL phase2 */
%{
#define YY_NO_UNPUT
#include <stdio.h>
#include <stdlib.h>
void yyerror(const char *s);
extern int line_count;
extern char* yytext;
FILE * yyin;
%}

%union{
  /* put your types here */
  char* ident_val;
  int num_val;
}

%error-verbose
  /* %locations */
%start Program

%token <ident_val> IDENT
%token <num_val> NUMBER

%left EQ
%left ASSIGN
%left PLUS
%left SUB
%left MULT
%left DIV

%token L_PAREN
%token R_PAREN
%token SEMICOLON 
%token COLON
%left LT
%left LTE
%left GT
%left GTE
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET

%left MOD
%left NEQ
%token COMMA
%token IF
%token ENDIF
%token WHILE
%token THEN
%token FUNCTION
%token INTEGER
%token READ
%token WRITE
%token ARRAY
%token OF
%token CONTINUE
%token BREAK
%right NOT
%token TRUE
%token FALSE
%token RETURN
%token BEGINLOOP
%token ENDLOOP
%token BEGIN_LOCALS
%token END_LOCALS
%token BEGIN_PARAMS
%token END_PARAMS
%token BEGIN_BODY
%token END_BODY
%token ELSE
%token DO

/* %start program */

%% 

  /* write your rules here */
  Program:  %empty
    {printf("Program -> epsilon\n");}
      | Function Program
		{printf("Program -> Function Program\n");}
;

Function: FUNCTION Ident SEMICOLON BEGIN_PARAMS Declaration_Loop END_PARAMS BEGIN_LOCALS Declaration_Loop END_LOCALS BEGIN_BODY Statement_Loop END_BODY
{printf("Function -> FUNCTION Ident SEMICOLON BEGIN_PARAMS Declaration_Loop END_PARAMS BEGIN_LOCALS Declaration_Loop END_LOCALS BEGIN_BODY Statement_Loop END_BODY\n");}
;

Declaration:  Ident_Loop COLON INTEGER
  {printf("Declaration -> Ident_Loop COLON INTEGER\n");}
    | Ident_Loop COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
  {printf("Declaration -> Ident_Loop COLON ARRAY L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET OF INTEGER;\n");}
;

Declaration_Loop: %empty
  {printf("Declaration_Loop -> epsilon\n");}
    | Declaration SEMICOLON Declaration_Loop
  {printf("Declaration_Loop -> Declaration SEMICOLON Declaration_Loop\n");}
;

Ident_Loop: Ident
  {printf("Ident_Loop -> Ident \n");}
    | Ident COMMA Ident_Loop
  {printf("Ident_Loop -> Ident COMMA Ident_Loop\n");}

Ident:  IDENT
  {printf("Ident -> IDENT %s \n", $1);}

Statement_Loop: Statement SEMICOLON Statement_Loop
  {printf("Statement_Loop -> Statement SEMICOLON Statement_Loop\n");}
    | Statement SEMICOLON
  {printf("Statement_Loop -> Statement SEMICOLON\n");}
;

Statement: Var ASSIGN Expression
  {printf("Statement -> Var ASSIGN Expression\n");}
    | IF BoolExp THEN Statement_Loop ElseStatement ENDIF
  {printf("Statement -> IF BoolExp THEN Statement_Loop ElseStatement ENDIF\n");}		 
    | WHILE BoolExp BEGINLOOP Statement_Loop ENDLOOP
	{printf("Statement -> WHILE BoolExp BEGINLOOP Statement_Loop ENDLOOP\n");}
    | DO BEGINLOOP Statement_Loop ENDLOOP WHILE BoolExp
	{printf("Statement -> DO BEGINLOOP Statement_Loop ENDLOOP WHILE BoolExp\n");}
    | READ Var
	{printf("Statement -> READ Var\n");}
    | WRITE Var
	{printf("Statement -> WRITE Var\n");}
    | CONTINUE
	{printf("Statement -> CONTINUE\n");}
    | BREAK
	{printf("Statement -> BREAK\n");}
    | RETURN Expression
	{printf("Statement -> RETURN Expression\n");}
;

ElseStatement:  %empty
  {printf("ElseStatement -> epsilon\n");}
    | ELSE Statement_Loop
	{printf("ElseStatement -> ELSE Statement_Loop\n");}
;

BoolExp:  NOT BoolExpression
  {printf("bool_exp -> NOT bool_exp\n");}
    | BoolExpression
  {printf("bool_exp -> bool_exp1\n");}

;
BoolExpression: Expression Comp Expression
  {printf("bool_exp -> Expression Comp Expression\n");}
    | TRUE
  {printf("bool_exp  -> TRUE\n");}
    | FALSE
  {printf("bool_exp  -> FALSE\n");}
    | L_PAREN BoolExp R_PAREN
  {printf("bool_exp  -> L_PAREN BoolExp R_PAREN\n");}
;

Comp: EQ
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

Expression: Mult_Expr
  {printf("Expression -> Mult_Expr\n");}
    | Mult_Expr PLUS Expression
  {printf("Expression -> Mult_Expr PLUS Expression\n");}
    | Mult_Expr SUB Expression
	{printf("Expression -> Mult_Expr SUB Expression\n");}
;

Expression_Loop:  %empty
  {printf("Expression_Loop -> epsilon\n");}
    | Expression COMMA Expression_Loop
  {printf("Expression_Loop -> Expression COMMA Expression_Loop\n");}
    | Expression
  {printf("Expression_Loop -> Expression\n");}
;

Mult_Expr:  Term
  {printf("Mult_Expr -> Term\n");}
    | Term MULT Mult_Expr
  {printf("Mult_Expr -> Term MULT Mult_Expr\n");}
  | Term DIV Mult_Expr
	{printf("Mult_Expr -> Term DIV Mult_Expr\n");}
    | Term MOD Mult_Expr
  {printf("Mult_Expr -> Term MOD Mult_Expr\n");}
;

Term: Var
  {printf("Term -> Var\n");}
    | NUMBER
  {printf("Term -> NUMBER\n");}
    | L_PAREN Expression R_PAREN
  {printf("Term -> L_PAREN Expression R_PAREN\n");}
    | Ident L_PAREN Expression_Loop R_PAREN
  {printf("Term -> Ident L_PAREN Expression_Loop R_PAREN\n");}
;

Var:  Ident L_SQUARE_BRACKET Expression R_SQUARE_BRACKET
  {printf("Var -> Ident  L_SQUARE_BRACKET Expression R_SQUARE_BRACKET\n");}
    | Ident
  {printf("Var -> Ident \n");}
;

%% 

/* int main(int argc, char **argv) {
   yyparse();
   return 0;
}  */

void yyerror(const char *s) {
    /* implement your error handling */
    printf("ERROR: %s at symbol \"%s\" on line %d\n", s, yytext, line_count);
    exit(1);
}

  int main(int argc, char* argv[]) {
    if (argc == 2) {
      yyin = fopen(argv[1], "r");
      if (yyin == 0) {
          printf("Error opening file: %s\n", argv[1]);
          exit(1);
      }
    } else {
      yyin = stdin;
    }

    yyparse();
  
    return 0;
  }  
