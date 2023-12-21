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

%start <Parsed_ast.decl list> prog
%%

(* a list of length >= 2 separated by 'sep' *)
tuple_sep(sep, X):
  | x1 = X; sep; x2 = X {[x1;x2]}
  | x = X; sep; xs = tuple_sep(sep, X) {x::xs}

prog:
 | p = terminated(list(decl), EOF) { p }


%inline infix_op:
  | ADD { Parsed_ast.ADD }
  | SUB { Parsed_ast.SUB }
  | MUL {Parsed_ast.MUL }
  | DIV { Parsed_ast.DIV }
  | EQ  { Parsed_ast.EQ }
  | LT  { Parsed_ast.LT }
  | GT { Parsed_ast.GT }
  | AND { Parsed_ast.AND }
  | OR { Parsed_ast.OR }

(* expressions in decreasing order of precedence *)

expr4:
  | i = INT { Parsed_ast.Int i }
  | i = LOWERCASE_IDENT {Parsed_ast.Ident i}
  | i = UPPERCASE_IDENT {Parsed_ast.Constr i}
  | TRUE { Parsed_ast.Bool true }
  | FALSE { Parsed_ast.Bool false}
  | UNIT { Parsed_ast.Unit }
  | LPAREN; e = expr; RPAREN { e }

expr3:
  | e = expr4 { e }
  | e = expr3; e2 = expr4 { Parsed_ast.App (e, e2)}

expr2:
 | e = expr3 {e}
 | e1 = expr2; op = infix_op; e2 = expr2 { Parsed_ast.Oper(e1, op, e2) }

expr:
 | e = expr2 { e }
 | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr {Parsed_ast.If(e1, e2, e3)}
 | FUN; i = LOWERCASE_IDENT; ARROW; e = expr; {Parsed_ast.Fun(i, e)}
 | MATCH; e = expr; WITH ; option(BAR); cl = separated_nonempty_list(BAR, case) {Parsed_ast.Match(e, cl)}
 | LPAREN; exprs = tuple_sep(COMMA, expr); RPAREN {Parsed_ast.Tuple (exprs)}
 | LET; i = LOWERCASE_IDENT; EQ; e1 = expr; IN; e2 = expr; {Parsed_ast.Let (i, e1, e2)}

pattern1:
  | i = INT { Parsed_ast.Pat_Int i }
  | i = LOWERCASE_IDENT {Parsed_ast.Pat_Ident i}
  | i = UPPERCASE_IDENT {Parsed_ast.Pat_Constr (i, None)}
  | TRUE { Parsed_ast.Pat_Bool true }
  | FALSE { Parsed_ast.Pat_Bool false}
  | UNIT { Parsed_ast.Pat_Unit }
  | UNDERSCORE {Parsed_ast.Pat_Any}
  | LPAREN; pats = tuple_sep(COMMA, pattern1); RPAREN {Parsed_ast.Pat_Tuple (pats)}
  | cname = UPPERCASE_IDENT; p = pattern1 {Parsed_ast.Pat_Constr (cname, Some p)}

pattern:
  | p = pattern1 { p }
  | p1 = pattern; BAR; p2 = pattern {Parsed_ast.Pat_Or (p1, p2)}

case:
  (* removes ambiguity of nested matches *)
  | p = pattern; ARROW; e = expr2 {(p, e)}

decl:
  | TYPE; tparams = loption(type_params) tname = LOWERCASE_IDENT; EQ; option(BAR); cl = separated_nonempty_list(BAR, type_constr) {Parsed_ast.Type(tparams, tname, cl)}
  | VAL; vname = LOWERCASE_IDENT; EQ; e = expr {Parsed_ast.Val (vname, e)}

type_constr:
  | tconstr = UPPERCASE_IDENT; {Parsed_ast.DeclConstr (tconstr, None) }
  | tconstr = UPPERCASE_IDENT; OF; texpr = type_expr { Parsed_ast.DeclConstr (tconstr, Some(texpr))}

(* do not require parenthesis when only one type parameter present *)

type_param:
  | APOSTROPHE; tparam = LOWERCASE_IDENT { tparam }

type_params:
  | tp = type_param { [tp] }
  | LPAREN; tps = tuple_sep(COMMA, type_param) RPAREN { tps }

type_expr:
  | TINT { Parsed_ast.TyInt }
  | TBOOL { Parsed_ast.TyBool }
  | TUNIT { Parsed_ast.TyUnit }
  | APOSTROPHE; tparam = LOWERCASE_IDENT { Parsed_ast.TyVar tparam}
  | tname = LOWERCASE_IDENT { Parsed_ast.TyCustom tname }
  | LPAREN; texprs = tuple_sep(MUL, type_expr) RPAREN { Parsed_ast.TyTuple texprs }
  | t1 = type_expr; ARROW; t2 = type_expr {Parsed_ast.TyFun (t1, t2)}