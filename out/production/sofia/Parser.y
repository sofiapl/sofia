-- # vim:syntax=yacc

{
module Parser (parseSofia) where

import Ast
import Token
}

%name parseSofia
%tokentype { Token }
%error { parsingError }
%monad { Either String } { (>>=) } { return }

%token
    T_NAME          { NameToken $$ }
    T_FUNCTION      { FunctionToken }
    T_RETURN        { ReturnToken }
    T_UNDEFINED     { UndefinedToken }
    '('             { LParenToken }
    ')'             { RParenToken }
    '{'             { LBraceToken }
    '}'             { RBraceToken }
    ','             { ColonToken }
    ';'             { SemiToken }

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
    : {- empty -}       { [] }
    | stmts stmt ';'    { $2 : $1 }

stmt
    : expr          { ExprStmt $1 }
    | T_RETURN expr { ReturnStmt $2 }

expr
    : '(' expr ')'  { $2 }
    | T_NAME        { NameExpr $1 }
    | T_UNDEFINED   { UndefinedExpr }
    | expr '(' exprs_list_non_req ')'   { CallExpr $1 (reverse $3) }
    | T_FUNCTION '(' names_list_non_req ')' '{' stmts '}' {
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
