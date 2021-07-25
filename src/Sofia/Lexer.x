-- # vim:syntax=lex

{
module Sofia.Lexer (lexSofia) where

import Data.Char (isHexDigit, chr, digitToInt)

import Sofia.Token
}

%wrapper "monad"

$w = [a-zA-Z_]
$W = [a-zA-Z_']
$d = [0-9]

tokens :-

$white+     ;
"//".*      ;
function    { constToken FunctionToken }
let         { constToken LetToken }
mut         { constToken MutToken }
if          { constToken IfToken }
else        { constToken ElseToken }
for         { constToken ForToken }
while       { constToken WhileToken }
return      { constToken ReturnToken }
undefined   { constToken UndefinedToken }
"("         { constToken LParenToken }
")"         { constToken RParenToken }
"{"         { constToken LBraceToken }
"}"         { constToken RBraceToken }
","         { constToken CommaToken }
";"         { constToken SemiToken }
"="         { constToken EqToken }
$w($W|$d)*  { strToken NameToken }
"'"."'"     { \(_, _, _, _:c:_) _ -> return $ TW $ CharLiteralToken c }
"-"?$d+     { \(_, _, _, str) -> return . TW . IntLiteralToken . read . flip take str }
$d*"."$d+   { \(_, _, _, str) -> return . TW . DoubleLiteralToken . read . ('0' :) . flip take str }
"-"?$d+"."$d+   { \(_, _, _, str) -> return . TW . DoubleLiteralToken . read . flip take str }

{
data TokenWrapper = TW Token | WEOF

alexEOF = return WEOF

constToken :: Token -> AlexAction TokenWrapper
constToken t _ _ = return $ TW t

strToken :: (String -> Token) -> AlexAction TokenWrapper
strToken f (_, _, _, str) = return . TW . f . flip take str

lexSofia :: String -> Either String [Token]
lexSofia str = reverse <$> runAlex str (lex' []) where
    lex' ts = alexMonadScan >>= lex'' where
        lex'' (TW t) = lex' (t:ts)
        lex'' WEOF   = return ts
}
