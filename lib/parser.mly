%token <int> INT
%token ADD, SUB, MUL, DIV
%token EOF

%left ADD, SUB
%left MUL, DIV


%start <Ast.expr> prog
%%

prog:
 | e = expr; EOF { e }

expr:
 | i = INT { Ast.INT i }
 | e1 = expr; ADD; e2 = expr { Ast.ADD (e1, e2) }
 | e1 = expr; SUB; e2 = expr { Ast.SUB (e1, e2) }
 | e1 = expr; MUL; e2 = expr { Ast.MUL (e1, e2) }
 | e1 = expr; DIV; e2 = expr { Ast.DIV (e1, e2) }