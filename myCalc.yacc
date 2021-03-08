

%%
(* required declarations*)
%name myCalc

%term 
    CONST | NOT | AND | OR | XOR | EQUALS
|   IMPLIES | IF | THEN | ELSE | LPAREN | RPAREN | EOF | TERM

%nonterm program | statement | formula | binop | temp

%pos int

(* optional declarations *)
%eop EOF
%noshift EOF

%right IF THEN ELSE 
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT

%start program 

%verbose

%%
program: temp statement (temp statement)
        | statement (statement)

temp: temp statement (temp statement)
    | statement (statement)
    
statement: formula TERM (formula TERM)

formula: CONST (CONST)
    |   NOT formula (NOT formula)
    |   formula binop formula (formula1 binop formula2)
    |   formula IMPLIES formula (formula1 IMPLIES formula2)
    |   IF formula THEN formula ELSE formula (IF formula1 THEN formula2 ELSE formula3)

binop: AND (AND)
    |  OR  (XOR)
    |  XOR (XOR)
    |  EQUALS (EQUALS)    