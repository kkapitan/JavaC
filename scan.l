%{
  #include <stdio.h>
  int yywrap(void);
  int yylex(void);
  #include "y.tab.h"
%}
%s Comment
%%
<INITIAL>"/*"                                   { BEGIN Comment; }
<Comment>"*/"                                   { BEGIN INITIAL; }
<Comment>.|\n                                   { ; }
<INITIAL>auto                                   { return KWD_auto; }
<INITIAL>break                                  { return KWD_break; }
<INITIAL>case                                   { return KWD_case; }
<INITIAL>char                                   { return KWD_char; }
<INITIAL>const                                  { return KWD_const; }
<INITIAL>continue                               { return KWD_continue; }
<INITIAL>default                                { return KWD_default; }
<INITIAL>do                                     { return KWD_do; }
<INITIAL>double                                 { return KWD_double; }
<INITIAL>else                                   { return KWD_else; }
<INITIAL>enum                                   { return KWD_enum; }
<INITIAL>extern                                 { return KWD_extern; }
<INITIAL>float                                  { return KWD_float; }
<INITIAL>for                                    { return KWD_for; }
<INITIAL>goto                                   { return KWD_goto; }
<INITIAL>if                                     { return KWD_if; }
<INITIAL>int                                    { return KWD_int; }
<INITIAL>long                                   { return KWD_long; }
<INITIAL>register                               { return KWD_register; }
<INITIAL>return                                 { return KWD_return; }
<INITIAL>short                                  { return KWD_short; }
<INITIAL>signed                                 { return KWD_signed; }
<INITIAL>sizeof                                 { return KWD_sizeof; }
<INITIAL>static                                 { return KWD_static; }
<INITIAL>struct                                 { return KWD_struct; }
<INITIAL>switch                                 { return KWD_switch; }
<INITIAL>typedef                                { return KWD_typedef; }
<INITIAL>union                                  { return KWD_union; }
<INITIAL>unsigned                               { return KWD_unsigned; }
<INITIAL>void                                   { return KWD_void; }
<INITIAL>volatile                               { return KWD_volatile; }
<INITIAL>while                                  { return KWD_while; }
<INITIAL>">>="                                  { return OP_SHR_ASSIGN; }
<INITIAL>"<<="                                  { return OP_SHL_ASSIGN; }
<INITIAL>"+="                                   { return OP_ADD_ASSIGN; }
<INITIAL>"-="                                   { return OP_SUB_ASSIGN; }
<INITIAL>"*="                                   { return OP_MUL_ASSIGN; }
<INITIAL>"/="                                   { return OP_DIV_ASSIGN; }
<INITIAL>"%="                                   { return OP_MOD_ASSIGN; }
<INITIAL>"&="                                   { return OP_AND_ASSIGN; }
<INITIAL>"^="                                   { return OP_XOR_ASSIGN; }
<INITIAL>"|="                                   { return OP_OR_ASSIGN; }
<INITIAL>">>"                                   { return OP_SHR; }
<INITIAL>"<<"                                   { return OP_SHL; }
<INITIAL>"++"                                   { return OP_INC; }
<INITIAL>"--"                                   { return OP_DEC; }
<INITIAL>"->"                                   { return OP_PTR; }
<INITIAL>"&&"                                   { return OP_AND; }
<INITIAL>"||"                                   { return OP_OR; }
<INITIAL>"<="                                   { return OP_LE; }
<INITIAL>">="                                   { return OP_GE; }
<INITIAL>"=="                                   { return OP_EQ; }
<INITIAL>"!="                                   { return OP_NE; }
<INITIAL>"..."                                  { return OP_ELIPSIS; }
<INITIAL>\;                                     { return ';'; }
<INITIAL>\{                                     { return '{'; }
<INITIAL>\}                                     { return '}'; }
<INITIAL>\,                                     { return ','; }
<INITIAL>\:                                     { return ':'; }
<INITIAL>\=                                     { return '='; }
<INITIAL>\(                                     { return '('; }
<INITIAL>\)                                     { return ')'; }
<INITIAL>\[                                     { return '['; }
<INITIAL>\]                                     { return ']'; }
<INITIAL>\.                                     { return '.'; }
<INITIAL>\&                                     { return '&'; }
<INITIAL>\!                                     { return '!'; }
<INITIAL>\~                                     { return '~'; }
<INITIAL>\+                                     { return '+'; }
<INITIAL>\-                                     { return '-'; }
<INITIAL>\*                                     { return '*'; }
<INITIAL>\/                                     { return '/'; }
<INITIAL>\%                                     { return '%'; }
<INITIAL>\<                                     { return '<'; }
<INITIAL>\>                                     { return '>'; }
<INITIAL>\^                                     { return '^'; }
<INITIAL>\|                                     { return '|'; }
<INITIAL>\?                                     { return '?'; }
<INITIAL>[+\-]?[0-9]+                           { 
                                                  yylval.text = strdup(yytext);
                                                  return CONST_INT;
                                                }
<INITIAL>[+\-]?0[xX][0-9a-fA-F]+                {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_INT;
                                                }
<INITIAL>[+\-]?[0-9]+[lL]                       {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_LONG_INT;
                                                }
<INITIAL>[+\-]?0[xX][0-9a-fA-F]+[lL]            { 
                                                  yylval.text = strdup(yytext);
                                                  return CONST_LONG_INT;
                                                }
<INITIAL>\+?[0-9]+[uU]                          {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_UNSIGNED;
                                                }
<INITIAL>\+?0[xX][0-9a-fA-F]+[uU]               {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_UNSIGNED;
                                                }
<INITIAL>\+?[0-9]+[uU][lL]                      {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_UNSIGNED_LONG_INT;
                                                }
<INITIAL>\+?[0-9]+[lL][uU]                      {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_UNSIGNED_LONG_INT;
                                                }
<INITIAL>\+?0[xX][0-9a-fA-F]+[uU][lL]           {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_UNSIGNED_LONG_INT;
                                                }
<INITIAL>\+?0[xX][0-9a-fA-F]+[lL][uU]           {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_UNSIGNED_LONG_INT;
                                                }
<INITIAL>[0-9]*\.[0-9]+([Ee][+\-]?[0-9]+)?      {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_DOUBLE;
                                                }
<INITIAL>[0-9]+\.[0-9]*([Ee][+\-]?[0-9]+)?      {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_DOUBLE;
                                                }
<INITIAL>[0-9]*\.[0-9]+([Ee][+\-]?[0-9]+)?[fF]  {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_FLOAT;
                                                }
<INITIAL>[0-9]+\.[0-9]*([Ee][+\-]?[0-9]+)?[fF]  {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_FLOAT;
                                                }
<INITIAL>[0-9]*\.[0-9]+([Ee][+\-]?[0-9]+)?[lL]  {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_LONG_DOUBLE;
                                                }
<INITIAL>[0-9]+\.[0-9]*([Ee][+\-]?[0-9]+)?[lL]  {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_LONG_DOUBLE;
                                                }
<INITIAL>\"(\\.|[^\\"])*\"                      {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_STRING;
                                                }
<INITIAL>'[0-9a-zA-Z]'|'\\n'|'\\t'|'\\v'        {
                                                  yylval.text = strdup(yytext);
                                                  return CONST_CHAR;
                                                }
<INITIAL>[a-zA-Z_][a-zA-Z_0-9]*                 {
                                                  yylval.text = strdup(yytext);
                                                  return IDENT; /* typedef -> check type */
                                                }
<INITIAL>[ \t\v\n\f]                            { ; }
%%
int yywrap(void) { return 1; }
