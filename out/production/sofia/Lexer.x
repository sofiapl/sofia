-- # vim:syntax=lex

{
module Lexer (lexSofia) where

import Data.Char (isHexDigit, chr, digitToInt)

import Token
}

%wrapper "monad"

$w = [a-zA-Z_']
$d = [0-9]

tokens :-

$white+     ;
"//".*      ;
export      { constToken ExportToken }
function    { constToken FunctionToken }
let         { constToken LetToken }
mut         { constToken MutToken }
if          { constToken IfToken }
else        { constToken ElseToken }
for         { constToken ForToken }
while       { constToken WhileToken }
when        { constToken WhenToken }
return      { constToken ReturnToken }
undefined   { constToken UndefinedToken }
"("         { constToken LParenToken }
")"         { constToken RParenToken }
"{"         { constToken LBraceToken }
"}"         { constToken RBraceToken }
","         { constToken CommaToken }
":"         { constToken ColonToken }
";"         { constToken SemiToken }
$w($w|$d)*  { strToken NameToken }

{
data TokenWrapper = TW Token | WEOF

alexEOF = return WEOF

constToken :: Token -> AlexAction TokenWrapper
constToken t _ _ = return $ TW t

strToken :: (String -> Token) -> AlexAction TokenWrapper
strToken f (_, _, _, str) len = return $ TW $ f $ take len str

lexSofia :: String -> Either String [Token]
lexSofia str = reverse <$> runAlex str (lex' []) where
    lex' ts = alexMonadScan >>= lex'' where
        lex'' (TW t) = lex' (t:ts)
        lex'' WEOF   = return ts
}
