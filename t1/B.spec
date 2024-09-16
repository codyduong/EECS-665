\"[^\"]*\" STRING true
'[^']' CHAR true
colou?r COLOR false
cat|dog ANIMAL true
(ab)+ TOKEN_AB_PLUS true
[0-9]+ NUMBER true
ab*c MATCH_AB_STAR true
a.b MATCH_A_DOT_B true
[a-zA-Z]+ IDENTIFIER true
[ \n] (SKIP)
. (ERR) "unknown char"