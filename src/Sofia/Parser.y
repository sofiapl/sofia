-- # vim:syntax=yacc

{
module Sofia.Parser (parseSofia) where

import Sofia.Ast
import Sofia.Token
}

%name parseSofia
%tokentype { Token }
%error { parsingError }
%monad { Either String } { (>>=) } { return }

%token
    T_NAME          { NameToken $$ }
    T_CHAR_LITERAL  { CharLiteralToken $$ }
    T_INT_LITERAL   { IntLiteralToken $$ }
    T_DOUBLE_LITERAL    { DoubleLiteralToken $$ }
    T_LET           { LetToken }
    T_MUT           { MutToken }
    T_IF            { IfToken }
    T_ELSE          { ElseToken }
    T_FUNCTION      { FunctionToken }
    T_RETURN        { ReturnToken }
    T_UNDEFINED     { UndefinedToken }
    '('             { LParenToken }
    ')'             { RParenToken }
    '{'             { LBraceToken }
    '}'             { RBraceToken }
    ','             { CommaToken }
    ';'             { SemiToken }
    '='             { EqToken }

%left ','

%%

program
    : top_stmts     { reverse $1 }

top_stmts
    : {- empty -}           { [] }
    | top_stmts top_stmt    { $2 : $1 }

top_stmt
    : T_FUNCTION T_NAME '(' names_list_non_req ')' '{' stmts '}'    {
        FunctionStmt $2 (reverse $4) (reverse $7)
    }
    | T_LET T_NAME '=' expr ';' {
        GlobalLetStmt $2 $4
    }

names_list_non_req
    : {- empty -}       { [] }
    | names_list_req    { $1 }

names_list_req
    : T_NAME                    { [$1] }
    | names_list_req ',' T_NAME { $3 : $1 }

stmts
    : {- empty -}   { [] }
    | stmts stmt    { $2 : $1 }

stmt
    : expr ';'                          { ExprStmt $1 }
    | T_RETURN expr ';'                 { ReturnStmt $2 }
    | T_LET T_NAME '=' expr ';'         { LetStmt $2 $4 }
    | T_LET T_MUT T_NAME '=' expr ';'   { LetMutStmt $3 $5 }
    | T_IF '(' expr ')' '{' stmts '}'   { IfStmt $3 $6 }
    | T_IF '(' expr ')' '{' stmts '}' T_ELSE '{' stmts '}'  { IfElseStmt $3 $6 $10 }

expr
    : '(' expr ')'  { $2 }
    | T_NAME        { NameExpr $1 }
    | T_CHAR_LITERAL    { CharLiteralExpr $1 }
    | T_INT_LITERAL { IntLiteralExpr $1 }
    | T_DOUBLE_LITERAL  { DoubleLiteralExpr $1 }
    | T_UNDEFINED   { UndefinedExpr }
    | T_NAME '=' expr   { SetExpr $1 $3 }
--    | T_IF '(' expr ')' '{' stmts '}' T_ELSE '{' stmts '}'  { IfExpr $3 $6 $10 }
    | expr '(' exprs_list_non_req ')'   { CallExpr $1 (reverse $3) }
    | T_FUNCTION '(' names_list_non_req ')' '{' stmts '}'   {
        FunctionExpr (reverse $3) (reverse $6)
    }

exprs_list_non_req
    : {- empty -}       { [] }
    | exprs_list_req    { $1 }

exprs_list_req
    : expr                      { [$1] }
    | exprs_list_req ',' expr   { $3 : $1 }

{
parsingError ts = Left $ "Parsing error at " ++ show ts
}
