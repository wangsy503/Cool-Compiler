/*
 *  cool.y
 *              Parser definition for the COOL language.
 *
 */
%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

/* Add your own C declarations here */


/************************************************************************/
/*                DONT CHANGE ANYTHING IN THIS SECTION                  */

extern int yylex();           /* the entry point to the lexer  */
extern int curr_lineno;
extern char *curr_filename;
Program ast_root;            /* the result of the parse  */
Classes parse_results;       /* for use in semantic analysis */
int omerrs = 0;              /* number of errors in lexing and parsing */

/*
   The parser will always call the yyerror function when it encounters a parse
   error. The given yyerror implementation (see below) justs prints out the
   location in the file where the error was found. You should not change the
   error message of yyerror, since it will be used for grading puproses.
*/
void yyerror(const char *s);

/*
   The VERBOSE_ERRORS flag can be used in order to provide more detailed error
   messages. You can use the flag like this:

     if (VERBOSE_ERRORS)
       fprintf(stderr, "semicolon missing from end of declaration of class\n");

   By default the flag is set to 0. If you want to set it to 1 and see your
   verbose error messages, invoke your parser with the -v flag.

   You should try to provide accurate and detailed error messages. A small part
   of your grade will be for good quality error messages.
*/
extern int VERBOSE_ERRORS;

%}

/* A union of all the types that can be the result of parsing actions. */
%union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
  char *error_msg;
}

/* 
   Declare the terminals; a few have types for associated lexemes.
   The token ERROR is never used in the parser; thus, it is a parse
   error when the lexer returns it.

   The integer following token declaration is the numeric constant used
   to represent that token internally.  Typically, Bison generates these
   on its own, but we give explicit numbers to prevent version parity
   problems (bison 1.25 and earlier start at 258, later versions -- at
   257)
*/
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/
 
   /* Complete the nonterminal list below, giving a type for the semantic
      value of each non terminal. (See section 3.6 in the bison 
      documentation for details). */

/* Declare types for the grammar's non-terminals. */
%type <program> program
%type <classes> class_list
%type <class_> class

/* You will want to change the following line. */
%type <features> feature_list
%type <feature>  feature

%type <formals>  formal_list
%type <formal>   formal

%type <expressions> expression_list
%type <expressions> semicolon_expr_list
%type <expression> let_block
%type <expression> expression
%type <expression> if_assign

%type <cases>    case_list
%type <case_>    case

/* Precedence declarations go here. */
%nonassoc IN
%right ASSIGN
%left NOT
%nonassoc LE '<' '='
%left '+' '-'
%left '*' '/'
%left ISVOID
%left '~'
%left '@'
%left '.'


%%
/* 
   Save the root of the abstract syntax tree in a global variable.
*/
program : class_list 
                { ast_root = program($1); }
        ;

class_list
        : class            
                /* single class */
                { $$ = single_Classes($1); }
        | class_list class 
                /* several classes */
                { $$ = append_Classes($1,single_Classes($2)); }
        ;

/* If no parent is specified, the class inherits from the Object class. */
class  
        : CLASS TYPEID '{' feature_list '}' ';'
                { $$ = class_($2,idtable.add_string("Object"),$4,stringtable.add_string(curr_filename)); }
        | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
                { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
        | error ';'
                /* Parser should recover from a properly terminated class */
                { $$ = NULL; }
        ;

/* Feature list may be empty, but no empty features in list. */
feature_list
        :       
                /* empty */
                { $$ = nil_Features(); }
        | feature_list feature
                /* or more than one */
                { $$ = append_Features($1, single_Features($2)); }
        ;

feature
        : OBJECTID '(' formal_list ')' ':' TYPEID '{' expression '}' ';'
                /* method */
                { $$ = method($1,$3,$6,$8); }
        | OBJECTID ':' TYPEID ';'
                /* attributes, no assignment */
                { $$ = attr($1,$3,no_expr()); }
        | OBJECTID ':' TYPEID ASSIGN expression ';'
                /* attributes */
                { $$ = attr($1,$3,$5); }
        | error ';'
                /* Parser should recover from errors in features 
                   (going on to the next feature) */
                { $$ = NULL; }
        ;

/* Formal list may be empty */
formal_list
        :       
                /* empty */
                { $$ = nil_Formals(); }
        | formal
                /* Just one formal */
                { $$ = single_Formals($1); }
        | formal_list ',' formal
                /* More than one formal */
                { $$ = append_Formals($1,single_Formals($3)); }
        ;

formal
        : OBJECTID ':' TYPEID
                { $$ = formal($1,$3); }
        ;

/* Expression list may be empty */
expression_list
        :
                /* empty */
                { $$ = nil_Expressions(); }
        | expression
                /* Just one expression */
                { $$ = single_Expressions($1); }
        | expression_list ',' expression
                /* More than one expression */
                { $$ = append_Expressions($1,single_Expressions($3)); }
        ;

semicolon_expr_list
        : expression ';'
                /* Just one expression */
                { $$ = single_Expressions($1); }
        | semicolon_expr_list expression ';'
                /* More than one expression */
                { $$ = append_Expressions($1,single_Expressions($2));}
        | semicolon_expr_list error ';'
                /* Program should recover from error in an expression inside a {...} block. */
                { $$ = append_Expressions($1,NULL); }
        | error ';'
                { $$ = NULL; }
        | error
                /* Some errors may not have ';' but still inside {...} */
                { $$ = NULL; }
        ;

expression
        : OBJECTID ASSIGN expression
                /* assign */
                { $$ = assign($1,$3); }
        | expression '.' OBJECTID '(' expression_list ')'
                /* normal dispatch */
                { $$ = dispatch($1,$3,$5); }
        | expression '@' TYPEID '.' OBJECTID '(' expression_list ')'
                /* static dispatch */
                { $$ = static_dispatch($1,$3,$5,$7); }
        | OBJECTID '(' expression_list ')'
                { $$ = dispatch(object(idtable.add_string("self")),$1,$3); }
        | IF expression THEN expression ELSE expression FI
                /* condition */
                { $$ = cond($2,$4,$6); }
        | WHILE expression LOOP expression POOL
                /* while loop */
                { $$ = loop($2,$4); }
        | '{' semicolon_expr_list '}'
                { $$ = block($2); }
        | LET let_block
                /* To make the question simple, we build a let_block */
                { $$ = $2; }
        | CASE expression OF case_list ESAC
                { $$ = typcase($2,$4); }
        | NEW TYPEID
                /* new */
                { $$ = new_($2); }
        | ISVOID expression
                /* isvoid */
                { $$ = isvoid($2); }

        /* operations */
        | expression '+' expression
                { $$ = plus($1,$3); }
        | expression '-' expression
                { $$ = sub($1,$3); }
        | expression '*' expression
                { $$ = mul($1,$3); }
        | expression '/' expression
                { $$ = divide($1,$3); }
        | '~' expression
                { $$ = neg($2); }
        | expression '<' expression
                { $$ = lt($1,$3); }
        | expression LE expression
                { $$ = leq($1,$3); }
        | expression '=' expression
                { $$ = eq($1,$3); }
        | NOT expression
                { $$ = comp($2); }
        | '(' expression ')'
                { $$ = $2; }

        /* const */
        | OBJECTID
                { $$ = object($1); }
        | INT_CONST
                { $$ = int_const($1); }
        | STR_CONST
                { $$ = string_const($1); }
        | BOOL_CONST
                { $$ = bool_const($1); }
        ;

let_block
        : OBJECTID ':' TYPEID if_assign IN expression
                { $$ = let($1,$3,$4,$6); }
        | OBJECTID ':' TYPEID if_assign ',' let_block
                { $$ = let($1,$3,$4,$6); }
        | error ',' let_block
                /* the parser should recover from errors in a let binding */
                { $$ = $3; }
        ;

if_assign
/* This can solve some line number problems. */
        : 
                /* empty */
                { $$ = no_expr(); }
        | ASSIGN expression
                /* <- expr */
                { $$ = $2; }
        ;


case_list      
        : case                          
                { $$ = single_Cases($1); }
        | case_list case
                { $$ = append_Cases($1,single_Cases($2)); }
        ;

case
        : OBJECTID ':' TYPEID DARROW expression ';'
                { $$ = branch($1,$3,$5); }
        ;


/* end of grammar */
%%

/* This function is called automatically when Bison detects a parse error. */
void yyerror(const char *s)
{
  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;

  if(omerrs>20) {
      if (VERBOSE_ERRORS)
         fprintf(stderr, "More than 20 errors\n");
      exit(1);
  }
}

