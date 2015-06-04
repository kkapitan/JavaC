%{
  int yylex(void);
  void yyerror(const char *,...);
  int yyparse(void);
  extern int yylineno;
#include <stdio.h>
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

%%

Program
  :  Input {printf("public class DefaultClass\n{\n    public static void main(String[] args)\n    {\n    }\n}");} 
  ;

Input
  : ExternalDefinition
  | Input ExternalDefinition
  ;
PrimaryExpr
  : IDENT
  | Constant
  | '(' Expr ')'
  ;
PostfixExpr
  : PrimaryExpr
  | PostfixExpr '[' Expr ']'
  | PostfixExpr '(' ')'
  | PostfixExpr '(' ArgumentExprList ')'
  | PostfixExpr '.' IDENT
  | PostfixExpr OP_PTR IDENT
  | PostfixExpr OP_INC
  | PostfixExpr OP_DEC
  ;
ArgumentExprList
  : AssignmentExpr
  | ArgumentExprList ',' AssignmentExpr
  ;
UnaryExpr
  : PostfixExpr
  | OP_INC UnaryExpr
  | OP_DEC UnaryExpr
  | UnaryOperator CastExpr
  | KWD_sizeof UnaryExpr
  | KWD_sizeof '(' TypeName ')'
  ;
UnaryOperator
  : '&'
  | '*'
  | '+'
  | '-'
  | '~'
  | '!'
  ;
CastExpr
  : UnaryExpr
  | '(' TypeName ')' CastExpr
  ;
MultiplicativeExpr
  : CastExpr
  | MultiplicativeExpr '*' CastExpr
  | MultiplicativeExpr '/' CastExpr
  | MultiplicativeExpr '%' CastExpr
  ;
AdditiveExpr
  : MultiplicativeExpr
  | AdditiveExpr '+' MultiplicativeExpr
  | AdditiveExpr '-' MultiplicativeExpr
  ;
ShiftExpr
  : AdditiveExpr
  | ShiftExpr OP_SHL AdditiveExpr
  | ShiftExpr OP_SHR AdditiveExpr
  ;
RelationalExpr
  : ShiftExpr
  | RelationalExpr '<' ShiftExpr
  | RelationalExpr '>' ShiftExpr
  | RelationalExpr OP_LE ShiftExpr
  | RelationalExpr OP_GE ShiftExpr
  ;
EqualityExpr
  : RelationalExpr
  | EqualityExpr OP_EQ RelationalExpr
  | EqualityExpr OP_NE RelationalExpr
  ;
AndExpr
  : EqualityExpr
  | AndExpr '&' EqualityExpr
  ;
ExclusiveOrExpr
  : AndExpr
  | ExclusiveOrExpr '^' AndExpr
  ;
InclusiveOrExpr
  : ExclusiveOrExpr
  | InclusiveOrExpr '|' ExclusiveOrExpr
  ;
LogicalAndExpr
  : InclusiveOrExpr
  | LogicalAndExpr OP_AND InclusiveOrExpr
  ;
LogicalOrExpr
  : LogicalAndExpr
  | LogicalOrExpr OP_OR LogicalAndExpr
  ;
ConditionalExpr
  : LogicalOrExpr
  | LogicalOrExpr '?' LogicalOrExpr ':' ConditionalExpr
  ;
AssignmentExpr
  : ConditionalExpr
  | UnaryExpr AssignmentOperator AssignmentExpr
  ;
AssignmentOperator
  : '='
  | OP_MUL_ASSIGN
  | OP_DIV_ASSIGN
  | OP_MOD_ASSIGN
  | OP_ADD_ASSIGN
  | OP_SUB_ASSIGN
  | OP_SHL_ASSIGN
  | OP_SHR_ASSIGN 
  | OP_AND_ASSIGN
  | OP_XOR_ASSIGN 
  | OP_OR_ASSIGN
  ;
Expr
  : AssignmentExpr
  | Expr ',' AssignmentExpr
  ;
ConstantExpr
  : ConditionalExpr
  ;
Declaration
  : DeclarationSpecifiers ';'
  | DeclarationSpecifiers InitDeclaratorList ';'
  ;
DeclarationSpecifiers
  : StorageClassSpecifier
  | StorageClassSpecifier DeclarationSpecifiers
  | TypeSpecifier
  | TypeSpecifier DeclarationSpecifiers
  ;
InitDeclaratorList
  : InitDeclarator
  | InitDeclaratorList ',' InitDeclarator
  ;
InitDeclarator
  : Declarator
  | Declarator '=' Initializer
  ;
StorageClassSpecifier
  : KWD_typedef
  | KWD_extern
  | KWD_static
  | KWD_auto
  | KWD_register
  ;
TypeSpecifier
  : KWD_char
  | KWD_short
  | KWD_int
  | KWD_long
  | KWD_signed
  | KWD_unsigned
  | KWD_float
  | KWD_double
  | KWD_const
  | KWD_volatile
  | KWD_void
  | StructOrUnionSpecifier
  | EnumSpecifier
  | TYPE_NAME
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
  | Pointer Declarator2
  ;
Declarator2
  : IDENT
  | '(' Declarator ')'
  | Declarator2 '[' ']'
  | Declarator2 '[' ConstantExpr ']'
  | Declarator2 '(' ')'
  | Declarator2 '(' ParameterTypeList ')'
  | Declarator2 '(' ParameterIdentifierList ')'
  ;
Pointer
  : '*'
  | '*' TypeSpecifierList
  | '*' Pointer
  | '*' TypeSpecifierList Pointer
  ;
TypeSpecifierList
  : TypeSpecifier
  | TypeSpecifierList TypeSpecifier
  ;
ParameterIdentifierList
  : IdentifierList
  | IdentifierList ',' OP_ELIPSIS
  ;
IdentifierList
  : IDENT
  | IdentifierList ',' IDENT
  ;
ParameterTypeList
  : ParameterList
  | ParameterList ',' OP_ELIPSIS
  ;
ParameterList
  : ParameterDeclaration
  | ParameterList ',' ParameterDeclaration
  ;
ParameterDeclaration
  : TypeSpecifierList Declarator
  | TypeName
  ;
TypeName
  : TypeSpecifierList
  | TypeSpecifierList AbstractDeclarator
  ;
AbstractDeclarator
  : Pointer
  | AbstractDeclarator2
  | Pointer AbstractDeclarator2
  ;
AbstractDeclarator2
  : '(' AbstractDeclarator ')'
  | '[' ']'
  | '[' ConstantExpr ']'
  | AbstractDeclarator2 '[' ']'
  | AbstractDeclarator2 '[' ConstantExpr ']'
  | '(' ')'
  | '(' ParameterTypeList ')'
  | AbstractDeclarator2 '(' ')'
  | AbstractDeclarator2 '(' ParameterTypeList ')'
  ;
Initializer
  : AssignmentExpr
  | '{' InitializerList '}'
  | '{' InitializerList ',' '}'
  ;
InitializerList
  : Initializer
  | InitializerList ',' Initializer
  ;
Statement
  : LabeledStatement
  | CompoundStatement
  | ExpressionStatement
  | SelectionStatement
  | IterationStatement
  | JumpStatement
  ;
LabeledStatement
  : IDENT ':' Statement
  | KWD_case ConstantExpr ':' Statement
  | KWD_default ':' Statement
  ;
CompoundStatement
  : '{' '}'
  | '{' StatementList '}'
  | '{' DeclarationList '}'
  | '{' DeclarationList StatementList '}'
  ;
DeclarationList
  : Declaration
  | DeclarationList Declaration
  ;
StatementList
  : Statement
  | StatementList Statement
  ;
ExpressionStatement
  : ';'
  | Expr ';'
  ;
SelectionStatement
  : KWD_if '(' Expr ')' Statement %prec KWD_else
  | KWD_if '(' Expr ')' Statement KWD_else Statement
  | KWD_switch '(' Expr ')' Statement
  ;
IterationStatement
  : KWD_while '(' Expr ')' Statement
  | KWD_do Statement KWD_while '(' Expr ')' ';'
  | KWD_for '(' ';' ';' ')' Statement
  | KWD_for '(' ';' ';' Expr ')' Statement
  | KWD_for '(' ';' Expr ';' ')' Statement
  | KWD_for '(' ';' Expr ';' Expr ')' Statement
  | KWD_for '(' Expr ';' ';' ')' Statement
  | KWD_for '(' Expr ';' ';' Expr ')' Statement
  | KWD_for '(' Expr ';' Expr ';' ')' Statement
  | KWD_for '(' Expr ';' Expr ';' Expr ')' Statement
  ;
JumpStatement
  : KWD_goto IDENT ';'
  | KWD_continue ';'
  | KWD_break ';'
  | KWD_return ';'
  | KWD_return Expr ';'
  ;
ExternalDefinition
  : FunctionDefinition
  | Declaration
  ;
FunctionDefinition
  : Declarator FunctionBody
  | DeclarationSpecifiers Declarator FunctionBody
  ;
FunctionBody
  : CompoundStatement
  | DeclarationList CompoundStatement
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
