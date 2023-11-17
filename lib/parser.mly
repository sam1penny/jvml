%token <int> INT
%token <string> LOWERCASE_IDENT
%token <string> UPPERCASE_IDENT

%token LPAREN, RPAREN
%token TRUE, FALSE, AND, OR, IF, THEN, ELSE, EQ, LT, GT
%token FUN, ARROW, MATCH, BAR, WITH, UNDERSCORE
%token UNIT, COMMA, APOSTROPHE, LET, IN
%token CONS, EMPTY_LIST
%token EOF

%token TYPE, TINT, TBOOL, TUNIT, OF
%token VAL

%token ADD, SUB, MUL, DIV

%left BAR
%left ADD, SUB
%left MUL, DIV, AND, OR, EQ, LT, GT, ARROW

(*
%nonassoc THEN
%nonassoc ELSE
*)

%start <Ast.decl list> prog
%%

(* a list of length >= 2 separated by 'sep' *)
tuple_sep(sep, X):
  | x1 = X; sep; x2 = X {[x1;x2]}
  | x = X; sep; xs = tuple_sep(sep, X) {x::xs}

prog:
 | p = terminated(list(decl), EOF) { p }


constant:
 | i = INT { Ast.Int i }
 | i = LOWERCASE_IDENT {Ast.Ident i}
 | i = UPPERCASE_IDENT {Ast.Constr i}
 | TRUE { Ast.Bool true }
 | FALSE { Ast.Bool false}
 | UNIT { Ast.Unit }


%inline infix_op:
  | ADD { Ast.ADD }
  | SUB { Ast.SUB }
  | MUL {Ast.MUL }
  | DIV { Ast.DIV }
  | EQ  { Ast.EQ }
  | LT  { Ast.LT }
  | GT { Ast.GT }
  | AND { Ast.AND }
  | OR { Ast.OR }

(* expressions in decreasing order of precedence *)

expr4:
  | c = constant {Ast.Expr_Const c}
  | LPAREN; e = expr; RPAREN { e }

expr3:
  | e = expr4 { e }
  | e = expr3; e2 = expr4 { Ast.App (e, e2)}

expr2:
 | e = expr3 {e}
 | e1 = expr2; op = infix_op; e2 = expr2 { Ast.Oper(e1, op, e2) }

expr:
 | e = expr2 { e }
 | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr {Ast.If(e1, e2, e3)}
 | FUN; i = LOWERCASE_IDENT; ARROW; e = expr; {Ast.Fun(i, e)}
 | MATCH; e = expr; WITH ; option(BAR); cl = separated_nonempty_list(BAR, case) {Ast.Match(e, cl)}
 | LPAREN; exprs = tuple_sep(COMMA, expr); RPAREN {Ast.Tuple (exprs)}
 | LET; i = LOWERCASE_IDENT; EQ; e1 = expr; IN; e2 = expr; {Ast.Let (i, e1, e2)}

pattern1:
  | c = constant {Ast.Pat_Const c}
  | UNDERSCORE {Ast.Pat_Any}
  | LPAREN; pats = tuple_sep(COMMA, pattern1); RPAREN {Ast.Pat_Tuple (pats)}
  | cname = UPPERCASE_IDENT; p = pattern1 {Ast.Pat_Constr (cname, p)}

pattern:
  | p = pattern1 { p }
  | p1 = pattern; BAR; p2 = pattern {Ast.Pat_Or (p1, p2)}

case:
  (* removes ambiguity of nested matches *)
  | p = pattern; ARROW; e = expr2 {Ast.Case (p, e)}

decl:
  | TYPE; tparams = loption(type_params) tname = LOWERCASE_IDENT; EQ; option(BAR); cl = separated_nonempty_list(BAR, type_constr) {Ast.Type(tparams, tname, cl)}
  | VAL; vname = LOWERCASE_IDENT; EQ; e = expr {Ast.Val (vname, e)}

type_constr:
  | tconstr = UPPERCASE_IDENT; {Ast.DeclConstr (tconstr, None) }
  | tconstr = UPPERCASE_IDENT; OF; texpr = type_expr { Ast.DeclConstr (tconstr, Some(texpr))}

(* do not require parenthesis when only one type parameter present *)

type_param:
  | APOSTROPHE; tparam = LOWERCASE_IDENT { tparam }

type_params:
  | tp = type_param { [tp] }
  | LPAREN; tps = tuple_sep(COMMA, type_param) RPAREN { tps }

type_expr:
  | TINT { Ast.TINT }
  | TBOOL { Ast.TBOOL }
  | TUNIT { Ast.TUNIT }
  | APOSTROPHE; tparam = LOWERCASE_IDENT { Ast.TPARAM tparam}
  | tname = LOWERCASE_IDENT { Ast.TCUSTOM tname }
  | LPAREN; texprs = tuple_sep(MUL, type_expr) RPAREN { Ast.TTUPLE texprs }
  | t1 = type_expr; ARROW; t2 = type_expr {Ast.TFUN (t1, t2)}