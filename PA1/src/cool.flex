%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno; /* current line number */
extern YYSTYPE cool_yylval;

unsigned int nested_comment = 0; /* A variable to handle nested comments. */
unsigned int buf_in_use;  /* Record how much buffer is in use to prevent overflow. */
int check_overflow (unsigned int to_read_len) {
  if (buf_in_use + to_read_len + 1 > MAX_STR_CONST)
    return 1;
  return 0;
}

%}

%option noyywrap

DIGIT       [0-9]
INT         {DIGIT}+
TYPEID      [A-Z][_a-zA-Z0-9]*
OBJECTID    [a-z][_a-zA-Z0-9]*

/* Keywords */
CLASS       [cC][lL][aA][sS][sS]
ELSE        [eE][lL][sS][eE]
FI          [fF][iI]
IF          [iI][fF]
IN          [iI][nN]
INHERITS    [iI][nN][hH][eE][rR][iI][tT][sS]
ISVOID      [iI][sS][vV][oO][iI][dD]
LET         [lL][eE][tT]
LOOP        [lL][oO][oO][pP]
POOL        [pP][oO][oO][lL]
THEN        [tT][hH][eE][nN]
WHILE       [wW][hH][iI][lL][eE]
CASE        [cC][aA][sS][eE]
ESAC        [eE][sS][aA][cC]
NEW         [nN][eE][wW]
OF          [oO][fF]
NOT         [nN][oO][tT]
TRUE        [t][rR][uU][eE]
FALSE       [f][aA][lL][sS][eE]

/* Some symbols */
DARROW      =>
LE          <=
ASSIGN      <-

WHITESPACE  [ \f\r\t\v]+

%x MULTICOMMENT STRING ERRSTRING


%%

 /*
  * Define regular expressions for the tokens of COOL here. Make sure, you
  * handle correctly special cases, like:
  *   - Nested comments
  *   - String constants: They use C like systax and can contain escape
  *     sequences. Escape sequence \c is accepted for all characters c. Except
  *     for \n \t \b \f, the result is c.
  *   - Keywords: They are case-insensitive except for the values true and
  *     false, which must begin with a lower-case letter.
  *   - Multiple-character operators (like <-): The scanner should produce a
  *     single token for every such operator.
  *   - Line counting: You should keep the global variable curr_lineno updated
  *     with the correct line number
  */

 /* To handle nested comments. 
  * We use a counter 'nested_comment' to record nests.
  */
<INITIAL>"(*" {
  ++nested_comment; 
  BEGIN MULTICOMMENT;
}
<INITIAL>"*)" {
  cool_yylval.error_msg = "Unmatched *)"; 
  return ERROR;
}
<MULTICOMMENT>"(*"  ++nested_comment; 
<MULTICOMMENT>"*)"  {
  --nested_comment; 
  if(nested_comment == 0) 
    BEGIN INITIAL;
}
<MULTICOMMENT><<EOF>>  {
  cool_yylval.error_msg = "EOF in comment";
  BEGIN INITIAL;
  return ERROR;
}
<MULTICOMMENT>[^\n]  /* eat up anything else except for \n */
<MULTICOMMENT>\n  ++curr_lineno;  /* Meet newline inside muitiline comment */

 /* To handle string */
<INITIAL>\" {
  string_buf_ptr = string_buf;
  buf_in_use = 0;
  BEGIN STRING;
}
 /* End of a string */
<STRING>\"  {
  *string_buf_ptr = '\0';
  cool_yylval.symbol = stringtable.add_string(string_buf);
  BEGIN INITIAL;
  return STR_CONST;
}
 /* correct newline */
<STRING>"\\\n"  {
  ++curr_lineno;
  if (check_overflow(1)) {
    cool_yylval.error_msg = "String constant too long";
    BEGIN ERRSTRING;
    return ERROR;
  }
  *string_buf_ptr = '\n';
  string_buf_ptr++;
  buf_in_use++;
}
 /* Unescaped newline */
<STRING>"\n"  { 
  ++curr_lineno;
  yylval.error_msg = "Unterminated string constant";
  BEGIN INITIAL;
  return ERROR;
}
 /* EOF inside string */
<STRING><<EOF>>  {
  cool_yylval.error_msg = "EOF in string constant";
  BEGIN INITIAL;
  return ERROR;
}
 /* If null is encountered, we should report 
  * error and enter ERRSTRING mode. */
<STRING>\\?\0  {
  cool_yylval.error_msg = "String contains null character.";
  BEGIN ERRSTRING;
  return ERROR;
}
 /* Escape sequence \c is accepted for all characters c. 
 Except for \n \t \b \f, the result is c. If \null, we 
 report error (has been handled above). */
<STRING>"\\"[btnf] {
  if (check_overflow(1)) {
    cool_yylval.error_msg = "String constant too long";
    BEGIN ERRSTRING;
    return ERROR;
  }
  if (yytext[1] == 'b') *string_buf_ptr = '\b';
  if (yytext[1] == 't') *string_buf_ptr = '\t';
  if (yytext[1] == 'n') *string_buf_ptr = '\n';
  if (yytext[1] == 'f') *string_buf_ptr = '\f';
  string_buf_ptr++;
  buf_in_use++;
}
<STRING>"\\"[^btnf] {
  if (check_overflow(1)) {
    cool_yylval.error_msg = "String constant too long";
    BEGIN ERRSTRING;
    return ERROR;
  }
  strncpy(string_buf_ptr, yytext+1, 1);
  string_buf_ptr++;
  buf_in_use++;
}
<STRING>. {
  if (check_overflow(1)) {
    cool_yylval.error_msg = "String constant too long";
    BEGIN ERRSTRING;
    return ERROR;
  }
  strncpy(string_buf_ptr, yytext, 1);
  string_buf_ptr++;
  buf_in_use++;
}
 /* Handle ERRSTRING mode. */
 /* Encounter closing ". */
<ERRSTRING>[^\\]\" {
  BEGIN INITIAL;
}
 /* Encounter unescaped newline. */
<ERRSTRING>[^\\]\n {
  ++curr_lineno;
  BEGIN INITIAL;
}
<ERRSTRING>.  /* skip */
<ERRSTRING><<EOF>>  /* skip */

 /* eat up anything that is not new line. */
<INITIAL>"--"[^\n]* /* skip */
<INITIAL>\n       ++curr_lineno;
<INITIAL>{WHITESPACE} /* skip */


<INITIAL>{INT}    {
  cool_yylval.symbol = inttable.add_string(yytext);
  return INT_CONST;
}
<INITIAL>{TRUE}   {
  cool_yylval.boolean = true;
  return BOOL_CONST;
}
<INITIAL>{FALSE}   {
  cool_yylval.boolean = false;
  return BOOL_CONST;
}
<INITIAL>{CLASS}  return CLASS;
<INITIAL>{ELSE}   return ELSE;
<INITIAL>{FI}     return FI;
<INITIAL>{IF}     return IF;
<INITIAL>{IN}     return IN;
<INITIAL>{INHERITS} return INHERITS;
<INITIAL>{LET}    return LET;
<INITIAL>{LOOP}   return LOOP;
<INITIAL>{POOL}   return POOL;
<INITIAL>{THEN}   return THEN;
<INITIAL>{WHILE}  return WHILE;
<INITIAL>{CASE}   return CASE;
<INITIAL>{ESAC}   return ESAC;
<INITIAL>{OF}     return OF;
<INITIAL>{DARROW} return DARROW;
<INITIAL>{NEW}    return NEW;
<INITIAL>{ISVOID} return ISVOID;
<INITIAL>{LE}     return LE;
<INITIAL>{ASSIGN} return ASSIGN;
<INITIAL>{NOT}    return NOT;
<INITIAL>{TYPEID} {
  cool_yylval.symbol = idtable.add_string (yytext);
  return TYPEID;
}
<INITIAL>{OBJECTID} {
  cool_yylval.symbol = idtable.add_string (yytext);
  return OBJECTID;
}

<INITIAL>"+"      return '+';
<INITIAL>"-"      return '-';
<INITIAL>"*"      return '*';
<INITIAL>"/"      return '/';
<INITIAL>"."      return '.';
<INITIAL>"@"      return '@';
<INITIAL>"~"      return '~';
<INITIAL>"<"      return '<';
<INITIAL>"="      return '=';
<INITIAL>"{"      return '{';
<INITIAL>"}"      return '}';
<INITIAL>"("      return '(';
<INITIAL>")"      return ')';
<INITIAL>":"      return ':';
<INITIAL>";"      return ';';
<INITIAL>","      return ',';

<INITIAL>.  {
  yylval.error_msg = yytext;
  return ERROR;
}
%%
