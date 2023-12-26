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
%left MUL, DIV, AND, OR, EQ, LT, GT

%right ARROW

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
  | ADD { Common.ADD }
  | SUB { Common.SUB }
  | MUL {Common.MUL }
  | DIV { Common.DIV }
  | EQ  { Common.EQ }
  | LT  { Common.LT }
  | GT { Common.GT }
  | AND { Common.AND }
  | OR { Common.OR }

(* expressions in decreasing order of precedence *)

expr4:
  | i = INT { Parsed_ast.Int ($sloc, i) }
  | i = LOWERCASE_IDENT {Parsed_ast.Ident ($sloc, i)}
  | i = UPPERCASE_IDENT {Parsed_ast.Constr ($sloc, i)}
  | TRUE { Parsed_ast.Bool ($sloc, true) }
  | FALSE { Parsed_ast.Bool ($sloc, false)}
  | UNIT { Parsed_ast.Unit $sloc }
  | LPAREN; e = expr; RPAREN { e }

expr3:
  | e = expr4 { e }
  | e = expr3; e2 = expr4 { Parsed_ast.App ($sloc, e, e2)}

expr2:
 | e = expr3 {e}
 | e1 = expr2; op = infix_op; e2 = expr2 { Parsed_ast.Bop($sloc, e1, op, e2) }

expr:
 | e = expr2 { e }
 | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr {Parsed_ast.If($sloc, e1, e2, e3)}
 | FUN; i = LOWERCASE_IDENT; ARROW; e = expr; {Parsed_ast.Fun($sloc, i, e)}
 | MATCH; e = expr; WITH ; option(BAR); cl = separated_nonempty_list(BAR, case) {Parsed_ast.Match($sloc, e, cl)}
 | LPAREN; exprs = tuple_sep(COMMA, expr); RPAREN {Parsed_ast.Tuple ($sloc, exprs)}
 | LET; i = LOWERCASE_IDENT; EQ; e1 = expr; IN; e2 = expr; {Parsed_ast.Let ($sloc, i, e1, e2)}

pattern1:
  | i = INT { Parsed_ast.Pat_Int ($sloc, i) }
  | i = LOWERCASE_IDENT {Parsed_ast.Pat_Ident ($sloc, i)}
  | i = UPPERCASE_IDENT {Parsed_ast.Pat_Constr ($sloc, i, None)}
  | TRUE { Parsed_ast.Pat_Bool ($sloc, true) }
  | FALSE { Parsed_ast.Pat_Bool ($sloc, false)}
  | UNIT { Parsed_ast.Pat_Unit $sloc }
  | UNDERSCORE {Parsed_ast.Pat_Any $sloc}
  | LPAREN; pats = tuple_sep(COMMA, pattern1); RPAREN {Parsed_ast.Pat_Tuple ($sloc, pats)}
  | cname = UPPERCASE_IDENT; p = pattern1 {Parsed_ast.Pat_Constr ($sloc, cname, Some p)}

pattern:
  | p = pattern1 { p }
  | p1 = pattern; BAR; p2 = pattern {Parsed_ast.Pat_Or ($sloc, p1, p2)}

case:
  (* removes ambiguity of nested matches *)
  | p = pattern; ARROW; e = expr2 {(p, e)}

decl:
  | TYPE; tparams = loption(type_params); tname = LOWERCASE_IDENT; EQ; option(BAR); cl = separated_nonempty_list(BAR, type_constr) {Parsed_ast.Type($sloc, tparams, tname, cl)}
  | VAL; vname = LOWERCASE_IDENT; EQ; e = expr {Parsed_ast.Val ($sloc, vname, e)}

type_constr:
  | tconstr = UPPERCASE_IDENT; {Parsed_ast.DeclConstr ($sloc, tconstr, None) }
  | tconstr = UPPERCASE_IDENT; OF; texpr = type_expr { Parsed_ast.DeclConstr ($sloc, tconstr, Some(texpr))}

(* do not require parenthesis when only one type parameter present *)

type_param:
  | APOSTROPHE; tparam = LOWERCASE_IDENT { tparam }

type_params:
  | tp = type_param { [tp] }
  | LPAREN; tps = tuple_sep(COMMA, type_param) RPAREN { tps }

(* two levels handles int -> int tree ambiguity *)

type_expr2:
  | TINT { Parsed_ast.TyInt }
  | TBOOL { Parsed_ast.TyBool }
  | TUNIT { Parsed_ast.TyUnit }
  | tparam = type_param { Parsed_ast.TyVar tparam}
  | tname = LOWERCASE_IDENT { Parsed_ast.TyCustom ([], tname) }
  | texpr = type_expr2; tname = LOWERCASE_IDENT { Parsed_ast.TyCustom ([texpr], tname) }
  | LPAREN; texprs = tuple_sep(COMMA, type_expr); RPAREN; tname = LOWERCASE_IDENT {Parsed_ast.TyCustom (texprs, tname) }
  | LPAREN; texprs = tuple_sep(MUL, type_expr); RPAREN { Parsed_ast.TyTuple texprs }
  | LPAREN; texpr = type_expr; RPAREN { texpr }

type_expr:
  | texpr = type_expr2 { texpr }
  | t1 = type_expr; ARROW; t2 = type_expr {Parsed_ast.TyFun (t1, t2)}