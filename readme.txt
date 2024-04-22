Group Code: TEAMO023

Anurav Garg, 2021A7PS2782H
Dharanikanth Reddy, 2021A7PS0264H
Jason Aaron Goveas, 2021A7PS0237H
Pranav Dinesh Sharma, 2021A7PS2818H
Saksham Attri, 2021A7PS2950H


Instructions

Compilation

Task 1

lex TEAMO023.l
gcc lex.yy.c -lfl

Task 2
yacc -d TEAMO023.y
lex TEAMO023.l
gcc y.tab.c lex.yy.c -lfl

Running

./a.out file
