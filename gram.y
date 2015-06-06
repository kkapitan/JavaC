%{
  int yylex(void);
  void yyerror(const char *,...);
  int yyparse(void);
  extern int yylineno;
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define MAX_FUN 100

char* indentation_mark = "    ";

//Functions Queue
char* functions[MAX_FUN];
int functions_tail = 0;

int generate_javadoc = 1;
int used_function = 0;
int iLevel = 1;


char* addc(char* s1,char c1){
  int len = strlen(s1) + 2;
  char *buf = (char*)malloc(len);
  
  strcat(buf,s1); 
  buf[len-2] = c1; buf[len] = 0;
  return buf;
}


char* add(char* s1,char* s2){
  int len = strlen(s1) + strlen(s2) + 1;
  char *buf = (char*)malloc(len);
  
  strcat(buf,s1); 
  strcat(buf,s2);
  return buf;
}

char* add3(char* s1,char* s2,char* s3){
  return add(s1,add(s2,s3));
}

char* add4(char* s1,char* s2,char* s3,char *s4){
  return add3(s1,s2,add(s3,s4));
}

char* make_indent(char* statement,int num){
  while(num--)statement = add(indentation_mark,statement);
  return statement;
}

char* init_class(){
  return make_indent("DefaultClass ref = new DefaultClass();",iLevel+1);
}

char* make_javadoc(char* type,char *header){
    char* javadoc = "";
    javadoc = add(javadoc,make_indent("/**\n",iLevel));

    int len = strlen(header);
    char* parameter = "";
    for(int i = 0; i < len; i++){
        if(header[i] == '(' || header[i] == ' ' || header[i] == ',' || header[i] == ')'){
            if(header[i] != ' ' && strcmp(parameter,"")){
              if(header[i] == '('){
                  javadoc = add4(javadoc,make_indent(" * This method is generated from ",iLevel),parameter+1,"()\n");
              }else javadoc = add4(javadoc,make_indent(" * @param ",iLevel),parameter,add3(" is a translation of formal parameter ",parameter+1,"\n"));
            }
            parameter = "";
        }else parameter = addc(parameter,header[i]);
    }
    if(!strcmp(type,"int")){
      javadoc = add(javadoc,make_indent(" * @return some int value\n",iLevel));
    }
    javadoc = add(javadoc,make_indent(" */\n",iLevel));
    //printf("%s\n",javadoc);
    if(generate_javadoc)return javadoc;
    return "";
}

char* main_javadoc(){
    char* javadoc = "";
    javadoc = add(javadoc,make_indent("/**\n",iLevel));
    javadoc = add(javadoc,make_indent(" * This is an entry point for the application.\n",iLevel));
    javadoc = add(javadoc,make_indent(" * @param args the arguments for main function\n",iLevel));
    javadoc = add(javadoc,make_indent(" */\n",iLevel));
    if(generate_javadoc)return javadoc;
    return "";
}

char* author_and_version(char *autor,char* version){
  char* result = "";
  result = add(result,"/**\n");

  char* author = "@author";
  author = add(author,strstr(autor," "));  
  result = add3(result," * ",author);

  char* ver = "@version";
  ver = add(ver,strstr(version," "));
  result = add3(result," * ",ver);
  result = add(result," */\n");
  return result;
}

void push_function(char* ident){
  
  int identLen = strlen(ident) - strlen(strstr(ident,"("));
  char* buf = (char*)malloc(identLen+1);
  
  memcpy(buf,ident,identLen);
  buf[identLen] = 0;

  functions[functions_tail++] = buf;
}

int is_function(char* ident){
  for(int i = 0;i < functions_tail; i++){
      if(!strcmp(functions[i],ident))return 1;
  }
  return 0;
}



%}
%union
{
  int ival;
  char *text;
}

%token KWD_auto KWD_break KWD_case KWD_char KWD_const KWD_continue KWD_default KWD_do KWD_double
%right KWD_else
%token KWD_enum KWD_extern KWD_float KWD_for KWD_goto KWD_if KWD_int KWD_long
%token KWD_register KWD_return KWD_short KWD_signed KWD_sizeof KWD_static KWD_struct KWD_switch
%token KWD_typedef KWD_union KWD_unsigned KWD_void KWD_volatile KWD_while

%token OP_SHR_ASSIGN OP_SHL_ASSIGN OP_ADD_ASSIGN OP_SUB_ASSIGN OP_MUL_ASSIGN OP_DIV_ASSIGN
%token OP_MOD_ASSIGN OP_AND_ASSIGN OP_XOR_ASSIGN OP_OR_ASSIGN OP_SHR OP_SHL OP_INC OP_DEC
%token OP_PTR OP_AND OP_OR OP_LE OP_GE OP_EQ OP_NE OP_ELIPSIS

%token CONST_INT CONST_LONG_INT CONST_UNSIGNED CONST_UNSIGNED_LONG_INT CONST_DOUBLE CONST_FLOAT CONST_LONG_DOUBLE CONST_STRING CONST_CHAR

%token IDENT TYPE_NAME
//Own
%token INCLUDE_STATEMENT
%token AUTHOR VERSION

%type<text> IDENT PrimaryExpr PostfixExpr UnaryExpr ArgumentExprList Input CastExpr AssignmentExpr MultiplicativeExpr AdditiveExpr CompoundStatement
ShiftExpr RelationalExpr EqualityExpr AndExpr ExclusiveOrExpr InclusiveOrExpr LogicalOrExpr LogicalAndExpr ConditionalExpr Expr Statement ExpressionStatement StatementList DeclarationList
Declarator Declarator2 Declaration DeclarationSpecifiers InitDeclarator IdentifierList FunctionDefinition FunctionBody TypeSpecifier TYPE_NAME StorageClassSpecifier
OP_PTR OP_INC OP_DEC KWD_sizeof OP_GE TypeName UnaryOperator OP_LE OP_NE OP_SHL OP_SHR OP_SHL_ASSIGN OP_SHR_ASSIGN OP_EQ OP_AND OP_OR AssignmentOperator InitDeclaratorList
Pointer TypeSpecifierList ParameterIdentifierList ParameterTypeList ParameterDeclaration AbstractDeclarator2 AbstractDeclarator ParameterList Initializer ConstantExpr OP_ELIPSIS
InitializerList IterationStatement IfStatement EmptyStatement CompundStatementClosure CompoundStatementBody ExternalDefinition JumpStatement SourceFile Program AUTHOR VERSION SelectionStatement InlineStatement
%%

SourceFile
  :  AUTHOR VERSION Program {printf("%spublic class DefaultClass\n{\n%s}",author_and_version($1,$2),$3);}
  |  {generate_javadoc = 0;}Program {printf("public class DefaultClass\n{\n%s}",$2);} 
  ;
Program
  :  Includes Input {$$ = $2;}
  ;

Includes
  : INCLUDE_STATEMENT
  | Includes INCLUDE_STATEMENT
  | 
  ;

Input
  : ExternalDefinition ;
  | Input ExternalDefinition { $$ = add3($1,generate_javadoc ? "" : "\n",$2);}
  ;
PrimaryExpr
  : IDENT  {   
      if(!strcmp("puts",$1)) 
        $$ = "System.out.println"; 
      else if(is_function(add("$",$1))){
        $$ = add("ref.$",$1); 
        used_function = 1;
      } else $$ = add("$",$1); 
    }
  | Constant
  | '(' Expr ')'
  ;
PostfixExpr
  : PrimaryExpr 
  | PostfixExpr '[' Expr ']'                { $$ = add4($1,"[",$3,"]");}
  | PostfixExpr '(' ')'                     { $$ = add($1,"()");}
  | PostfixExpr '(' ArgumentExprList ')'    { $$ = add4($1,"(",$3,")");}
  | PostfixExpr '.' IDENT                   { $$ = add3($1,".",$3);}
  | PostfixExpr OP_PTR IDENT                { $$ = add3($1,$2,$3);}
  | PostfixExpr OP_INC                      { $$ = add($1,"++");}
  | PostfixExpr OP_DEC                      { $$ = add($1,"--");}
  ;
ArgumentExprList
  : AssignmentExpr                          
  | ArgumentExprList ',' AssignmentExpr     { $$ = add3($1,", ",$3);}
  ;
UnaryExpr
  : PostfixExpr
  | OP_INC UnaryExpr                        { $$ = add("++",$2);}
  | OP_DEC UnaryExpr                        { $$ = add("--",$2);}
  | UnaryOperator CastExpr                  { $$ = add($1,$2);}
  | KWD_sizeof UnaryExpr                    { $$ = add($1,$2);}
  | KWD_sizeof '(' TypeName ')'             { $$ = add4($1,"(",$3,")");}
  ;
UnaryOperator
  : '&'                                     { $$ = "&";}
  | '*'                                     { $$ = "*";}
  | '+'                                     { $$ = "+";}
  | '-'                                     { $$ = "-";}
  | '~'                                     { $$ = "~";}
  | '!'                                     { $$ = "!";}
  ;
CastExpr
  : UnaryExpr                               
  | '(' TypeName ')' CastExpr               { $$ = add4("(",$2,")",$4);}
  ;
MultiplicativeExpr
  : CastExpr
  | MultiplicativeExpr '*' CastExpr         { $$ = add3($1," * ",$3);}
  | MultiplicativeExpr '/' CastExpr         { $$ = add3($1," / ",$3);}
  | MultiplicativeExpr '%' CastExpr         { $$ = add3($1," % ",$3);}
  ;
AdditiveExpr
  : MultiplicativeExpr
  | AdditiveExpr '+' MultiplicativeExpr     { $$ = add3($1," + ",$3);}
  | AdditiveExpr '-' MultiplicativeExpr     { $$ = add3($1," - ",$3);}
  ;
ShiftExpr
  : AdditiveExpr
  | ShiftExpr OP_SHL AdditiveExpr           { $$ = add3($1,$2,$3);}
  | ShiftExpr OP_SHR AdditiveExpr           { $$ = add3($1,$2,$3);}
  ;
RelationalExpr
  : ShiftExpr
  | RelationalExpr '<' ShiftExpr            { $$ = add3($1," < ",$3);}
  | RelationalExpr '>' ShiftExpr            { $$ = add3($1," > ",$3);}
  | RelationalExpr OP_LE ShiftExpr          { $$ = add3($1," <= ",$3);}
  | RelationalExpr OP_GE ShiftExpr          { $$ = add3($1," >= ",$3);}
  ;
EqualityExpr
  : RelationalExpr
  | EqualityExpr OP_EQ RelationalExpr       { $$ = add3($1," == ",$3);}
  | EqualityExpr OP_NE RelationalExpr       { $$ = add3($1," != ",$3);}
  ;
AndExpr
  : EqualityExpr
  | AndExpr '&' EqualityExpr                { $$ = add3($1," & ",$3);}
  ;
ExclusiveOrExpr
  : AndExpr
  | ExclusiveOrExpr '^' AndExpr             { $$ = add3($1," ^ ",$3);}
  ;
InclusiveOrExpr
  : ExclusiveOrExpr
  | InclusiveOrExpr '|' ExclusiveOrExpr     { $$ = add3($1," | ",$3);}
  ;
LogicalAndExpr
  : InclusiveOrExpr
  | LogicalAndExpr OP_AND InclusiveOrExpr   { $$ = add3($1,$2,$3);}
  ;
LogicalOrExpr
  : LogicalAndExpr 
  | LogicalOrExpr OP_OR LogicalAndExpr      { $$ = add3($1,$2,$3);}
  ;
ConditionalExpr
  : LogicalOrExpr 
  | LogicalOrExpr '?' LogicalOrExpr ':' ConditionalExpr   { $$ = add4($1," ? ",$3,add(" : ",$5));}
  ;
AssignmentExpr
  : ConditionalExpr
  | UnaryExpr AssignmentOperator AssignmentExpr           { $$ = add3($1,$2,$3);}
  ;
AssignmentOperator
  : '='  { $$ = " = ";}
  | OP_MUL_ASSIGN                           { $$ = " *= ";}
  | OP_DIV_ASSIGN                           { $$ = " /= ";}
  | OP_MOD_ASSIGN                           { $$ = " %= ";}
  | OP_ADD_ASSIGN                           { $$ = " += ";}
  | OP_SUB_ASSIGN                           { $$ = " -= ";}
  | OP_SHL_ASSIGN                           { $$ = " >>= ";}
  | OP_SHR_ASSIGN                           { $$ = " <<= ";}
  | OP_AND_ASSIGN                           { $$ = " &= ";}
  | OP_XOR_ASSIGN                           { $$ = " ^= ";}
  | OP_OR_ASSIGN                            { $$ = " |= ";}
  ;
Expr
  : AssignmentExpr 
  | Expr ',' AssignmentExpr                 {$$ = add3($1,", ",$3);}
  ;
ConstantExpr
  : ConditionalExpr
  ;
Declaration
  : DeclarationSpecifiers ';'                     {$$ = add($1,";\n");}
  | DeclarationSpecifiers InitDeclaratorList ';'  {$$ = add3($1,$2,";\n");}
  ;
DeclarationSpecifiers
  : StorageClassSpecifier
  | StorageClassSpecifier DeclarationSpecifiers   {$$ = add3($1," ",$2);}
  | TypeSpecifier
  | TypeSpecifier DeclarationSpecifiers           {$$ = add3($1," ",$2);}
  ;
InitDeclaratorList
  : InitDeclarator                                  
  | InitDeclaratorList ',' InitDeclarator         {$$ = add3($1,",",$3);}
  ;
InitDeclarator
  : Declarator                                    {$$ = add(" ",$1);}
  | Declarator '=' Initializer                    {$$ = add4(" ",$1," = ",$3);}
  ;
StorageClassSpecifier
  : KWD_typedef
  | KWD_extern
  | KWD_static
  | KWD_auto
  | KWD_register
  ;
TypeSpecifier
  : KWD_char                                      { $$ = "char";}
  | KWD_short                                     { $$ = "short";}
  | KWD_int                                       { $$ = "int";}
  | KWD_long                                      { $$ = "long";}
  | KWD_signed                                    { $$ = "signed";}
  | KWD_unsigned                                  { $$ = "unsigned";}
  | KWD_float                                     { $$ = "float";}
  | KWD_double                                    { $$ = "double";}
  | KWD_const                                     { $$ = "const";}
  | KWD_volatile                                  { $$ = "volatile";}
  | KWD_void                                      { $$ = "void";}
  | KWD_char '*'                                  {$$="String";}
  | StructOrUnionSpecifier                        
  | EnumSpecifier
  | TYPE_NAME                                     { $$ = $1 ;}
  ;
StructOrUnionSpecifier
  : StructOrUnion IDENT '{' StructDeclarationList '}' 
  | StructOrUnion '{' StructDeclarationList '}'
  | StructOrUnion IDENT
  ;
StructOrUnion
  : KWD_struct
  | KWD_union
  ;

StructDeclarationList
  : StructDeclaration
  | StructDeclarationList StructDeclaration
  ;
StructDeclaration
  : TypeSpecifierList StructDeclaratorList ';'
  ;
StructDeclaratorList
  : StructDeclarator
  | StructDeclaratorList ',' StructDeclarator
  ;
StructDeclarator
  : Declarator
  | ':' ConstantExpr
  | Declarator ':' ConstantExpr
  ;
EnumSpecifier
  : KWD_enum '{' EnumeratorList '}'
  | KWD_enum IDENT '{' EnumeratorList '}'
  | KWD_enum IDENT
  ;
EnumeratorList
  : Enumerator
  | EnumeratorList ',' Enumerator
  ;
Enumerator
  : IDENT
  | IDENT '=' ConstantExpr
  ;


Declarator
  : Declarator2
  | Pointer Declarator2                         { $$ = add($1,$2);}
  ;

Declarator2
  : IDENT                                       { $$ = add("$",$1);} 
  | '(' Declarator ')'                          { $$ = add3("(",$2,")");} 
  | Declarator2 '[' ']'                         { $$ = add($1,"[]");} 
  | Declarator2 '[' ConstantExpr ']'            { $$ = add4($1,"[",$3,"]");} 
  | Declarator2 '(' ')'                         { $$ = add($1,"()");} 
  | Declarator2 '(' ParameterTypeList ')'       { $$ = add4($1,"(",$3,")");} 
  | Declarator2 '(' ParameterIdentifierList ')' { $$ = add4($1,"(",$3,")");} 
  ;
Pointer
  : '*'                                         { $$ = "*";}
  | '*' TypeSpecifierList                       { $$ = add("*",$2);}
  | '*' Pointer                                 { $$ = add("*",$2);}
  | '*' TypeSpecifierList Pointer               { $$ = add3("*",$2,$3);}
  ;
TypeSpecifierList
  : TypeSpecifier
  | TypeSpecifierList TypeSpecifier             { $$ = add($1,$2);}
  ;
ParameterIdentifierList
  : IdentifierList
  | IdentifierList ',' OP_ELIPSIS               { $$ = add3($1,", ",$3);}
  ;
IdentifierList
  : IDENT                                       { $$ = add("$",$1);}
  | IdentifierList ',' IDENT                    { $$ = add3($1,", $",$3);}
  ;
ParameterTypeList
  : ParameterList
  | ParameterList ',' OP_ELIPSIS                { $$ = add3($1,", ",$3);}
  ;
ParameterList
  : ParameterDeclaration
  | ParameterList ',' ParameterDeclaration      { $$ = add3($1,", ",$3);}
  ;
ParameterDeclaration
  : TypeSpecifierList Declarator                { $$ = add3($1," ",$2);}
  | TypeName
  ;
TypeName
  : TypeSpecifierList
  | TypeSpecifierList AbstractDeclarator        { $$ = add($1,$2);}
  ;
AbstractDeclarator
  : Pointer
  | AbstractDeclarator2
  | Pointer AbstractDeclarator2                 { $$ = add($1,$2);}
  ;
AbstractDeclarator2
  : '(' AbstractDeclarator ')'                      { $$ = add3("(",$2,")");}
  | '[' ']'                                         { $$ = "[]";} 
  | '[' ConstantExpr ']'                            { $$ = add3("[",$2,"]");}
  | AbstractDeclarator2 '[' ']'                     { $$ = add($1,"[]");}
  | AbstractDeclarator2 '[' ConstantExpr ']'        { $$ = add4($1,"[",$3,"]");}
  | '(' ')'                                         { $$ = "()";}
  | '(' ParameterTypeList ')'                       { $$ = add3("(",$2,")");}
  | AbstractDeclarator2 '(' ')'                     { $$ = add($1,"()");}  
  | AbstractDeclarator2 '(' ParameterTypeList ')'   { $$ = add4($1,"(",$3,")");}
  ;
Initializer
  : AssignmentExpr
  | '{' InitializerList '}'                         { $$ = add3("{",$2,"}");}
  | '{' InitializerList ',' '}'                     { $$ = add3("{",$2,",}");}
  ;
InitializerList
  : Initializer                                     
  | InitializerList ',' Initializer                 { $$ = add3($1,", ",$3);}
  ;
Statement
  : LabeledStatement
  | CompoundStatement
  | ExpressionStatement  
  | SelectionStatement
  | IterationStatement
  | JumpStatement      { if(!generate_javadoc)$$ = "";}                                

LabeledStatement
  : IDENT ':' Statement
  | KWD_case ConstantExpr ':' Statement
  | KWD_default ':' Statement
  ;

CompundStatementClosure 
  : '}'                                             {$$ = add3("\n",make_indent("{\n",iLevel-1),make_indent("}",iLevel-1));}
  | CompoundStatementBody '}'                       {$$ = add4("\n",make_indent("{\n",iLevel-1),$1,make_indent("}",iLevel-1));}   
  ;

CompoundStatement
  : '{' {iLevel++;} CompundStatementClosure             {$$ = add($3,"\n");iLevel--;}

CompoundStatementBody
  : StatementList                                        
  | DeclarationList
  | CompoundStatementBody DeclarationList {$$ = add($1,$2);}
  | CompoundStatementBody StatementList   {$$ = add($1,$2);}


DeclarationList
  : Declaration                                         {$$ = make_indent($1,iLevel);}
  | DeclarationList Declaration                         {$$ = add($1,make_indent($2,iLevel));}
StatementList
  : Statement 
  { 
    char* st = $1;
    if(strcmp("",$1) && *st != '\n'){
      $$ = make_indent($1,iLevel);
    }
  }
  | StatementList Statement 
  {
    char* st = $1;
    if(strcmp("",$2) && *st != '\n'){
      $$ = add($1,make_indent($2,iLevel));
    }else $$ = add($1,$2);
  }
  ;

EmptyStatement
  : ';'

InlineStatement
  : IterationStatement
  | ExpressionStatement
  | LabeledStatement
  | JumpStatement

IfStatement
  : KWD_if '(' Expr ')' EmptyStatement { $$ = add4(add("\n",make_indent("if (",iLevel)),$3,")\n",add(make_indent("{\n",iLevel),make_indent("}\n",iLevel)));}
  | KWD_if '(' Expr ')' InlineStatement { $$ = add4(add("\n",make_indent("if (",iLevel)),$3,")\n",add3(make_indent("{\n",iLevel),$5,make_indent("}\n",iLevel)));}
  | KWD_if '(' Expr ')' CompoundStatement { $$ = add4(add("\n",make_indent("if (",iLevel)),$3,")",$5);}

 
ExpressionStatement 
  : ';'                                                  { $$ = "Lalalalaadsadasda;\n";}
  | Expr ';'                                             {$$ = add($1,";\n");}
  ;
SelectionStatement
  : IfStatement                                     
  | IfStatement KWD_else SelectionStatement         {
              char* ifst = $3;
              while(*ifst == ' ' || *ifst == '\n')ifst++;
              $$ = add3($1,make_indent("else ",iLevel),ifst);
      }
  | IfStatement KWD_else EmptyStatement             {$$ = add($1,"else\n{\n}");}
  | IfStatement KWD_else InlineStatement            {$$ = add4($1,make_indent("else\n",iLevel),make_indent("{\n",iLevel),add(make_indent($3,iLevel+1),make_indent("}\n",iLevel)));}
  | IfStatement KWD_else CompoundStatement          {$$ = add3($1,make_indent("else",iLevel),$3);}
  | KWD_switch '(' Expr ')' Statement 
  ;
IterationStatement
  : KWD_while '(' Expr ')' Statement                      {$$ = add4("while (",$3,")",$5);}
  | KWD_do Statement KWD_while '(' Expr ')' ';'           {$$ = add4("do",$2,make_indent(add3("while (",$5,");"),iLevel),"\n");}
  | KWD_for '(' ';' ';' ')' Statement                     {$$ = add("for (;;)",$6);}
  | KWD_for '(' ';' ';' Expr ')' Statement                {$$ = add4("for (;; ",$5,")",$7);}
  | KWD_for '(' ';' Expr ';' ')' Statement                {$$ = add4("for (; ",$4,";)",$7);}
  | KWD_for '(' ';' Expr ';' Expr ')' Statement           {$$ = add4("for (; ",$4,"; ",add3($6,")",$8));}
  | KWD_for '(' Expr ';' ';' ')' Statement                {$$ = add4("for (",$3,";;)",$7);}
  | KWD_for '(' Expr ';' ';' Expr ')' Statement           {$$ = add4("for (",$3,";;",add3($6,")",$8));}
  | KWD_for '(' Expr ';' Expr ';' ')' Statement           {$$ = add4("for (",$3,"; ",add3($5,";)",$8));}
  | KWD_for '(' Expr ';' Expr ';' Expr ')' Statement      {$$ = add4("for (",$3,"; ",add4($5,"; ",$7,add(")",$9)));}
  ;
JumpStatement
  : KWD_goto IDENT ';'
  | KWD_continue ';'
  | KWD_break ';'
  | KWD_return ';'                                        {$$ = "return ;\n";}
  | KWD_return Expr ';'                                   {$$ = add3("return ",$2,";\n");}
  ;
ExternalDefinition
  : FunctionDefinition
  | Declaration
  ;
FunctionDefinition
  : DeclarationSpecifiers Declarator FunctionBody  
  {    
    if(!strcmp($2,"$main()")){
      if(used_function){
        char *f_body = $3;
        char *left_brace = strstr(f_body,"{");
        char *f_body_closure = left_brace+1;
        
        $$ = add(main_javadoc(),make_indent(add3("public static void main(String[] args)\n",add(make_indent("{\n",iLevel),init_class()),f_body_closure),iLevel));
      }else $$ = add(main_javadoc(),make_indent(add("public static void main(String[] args)",$3),iLevel));
    }
    else {
      push_function($2);
      
      $$ = add(make_javadoc($1,$2),make_indent(add4(add("public ",$1)," ",$2,$3),iLevel));
    }
  }
  ;
FunctionBody
  : CompoundStatement
  | DeclarationList CompoundStatement {$$ = add($1,$2);}
  ;

Constant : CONST_INT
         | CONST_LONG_INT
         | CONST_UNSIGNED
         | CONST_UNSIGNED_LONG_INT
         | CONST_DOUBLE
         | CONST_FLOAT
         | CONST_LONG_DOUBLE
         | CONST_STRING
         | CONST_CHAR
         ;
%%
void yyerror(const char *fmt, ...)
{
  printf("%s in line %d", fmt, yylineno);
}
int main() { return yyparse(); }
