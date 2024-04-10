%{
(* todo - consider including parser again *)
[@@@coverage exclude_file]
%}

%token <Int32.t> INT
%token <float> FLOAT
%token <string> LOWERCASE_IDENT
%token <string> UPPERCASE_IDENT

%token LPAREN, RPAREN
%token TRUE, FALSE, AND, OR, IF, THEN, ELSE, EQ, LT, GT
%token FUN, ARROW, MATCH, BAR, WITH, UNDERSCORE
%token UNIT, COMMA, APOSTROPHE, LET, IN, REC
%token DO, SEMICOLON, LCURLY, RCURLY, LSQUARE, RSQUARE
%token CONS, EMPTY_LIST
%token EOF

%token TYPE, TINT, TBOOL, TUNIT, OF
%token VAL

%token ADD, SUB, MUL, DIV
%token FLOAT_ADD, FLOAT_SUB, FLOAT_MUL, FLOAT_DIV

%left ADD, SUB, FLOAT_ADD, FLOAT_SUB
%left MUL, FLOAT_MUL, DIV, FLOAT_DIV, AND, OR, EQ, LT, GT

%right ARROW, CONS

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
  | FLOAT_ADD { Common.FLOAT_ADD }
  | FLOAT_SUB { Common.FLOAT_SUB }
  | FLOAT_MUL { Common.FLOAT_MUL }
  | FLOAT_DIV { Common.FLOAT_DIV }
  | EQ  { Common.EQ }
  | LT  { Common.LT }
  | GT { Common.GT }
  | AND { Common.AND }
  | OR { Common.OR }

(* expressions in decreasing order of precedence *)

expr4:
  | i = INT { Parsed_ast.Int ($sloc, i) }
  | f = FLOAT { Parsed_ast.Float ($sloc, f) }
  | i = LOWERCASE_IDENT {Parsed_ast.Ident ($sloc, i)}
  | i = UPPERCASE_IDENT {Parsed_ast.Constr ($sloc, i)}
  | EMPTY_LIST {Parsed_ast.Constr($sloc, "Nil$")}
  | TRUE { Parsed_ast.Bool ($sloc, true) }
  | FALSE { Parsed_ast.Bool ($sloc, false)}
  | UNIT { Parsed_ast.Unit $sloc }
  | LPAREN; e = expr; RPAREN { e }
  | LPAREN; exprs = tuple_sep(COMMA, expr); RPAREN {Parsed_ast.Tuple ($sloc, exprs)}
  | LSQUARE; e = expr; RSQUARE {
    Parsed_ast.App($sloc, Parsed_ast.Constr($sloc, "Cons$"), Parsed_ast.Tuple($sloc, [e; Parsed_ast.Constr($sloc, "Nil$")]))
    }
  | LSQUARE; exprs = tuple_sep(SEMICOLON, expr); RSQUARE {
    List.fold_right (fun e lst ->
    Parsed_ast.App(
      $sloc,
      Parsed_ast.Constr($sloc, "Cons$"),
      Parsed_ast.Tuple($sloc, [e; lst])
    )
    ) exprs (Parsed_ast.Constr($sloc, "Nil$"))
  }

expr3:
  | e = expr4 { e }
  | e = expr3; e2 = expr4 { Parsed_ast.App ($sloc, e, e2)}

expr2:
 | e = expr3 {e}
 | e1 = expr2; op = infix_op; e2 = expr2 { Parsed_ast.Bop($sloc, e1, op, e2) }
 | e1 = expr2; CONS; e2 = expr2 { Parsed_ast.App($sloc, Parsed_ast.Constr($sloc, "Cons$"), Parsed_ast.Tuple($sloc, [e1; e2])) }

expr:
 | e = expr2 { e }
 | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr {Parsed_ast.If($sloc, e1, e2, e3)}
 | FUN; i = LOWERCASE_IDENT; ARROW; e = expr; {Parsed_ast.Fun($sloc, i, e)}
 | MATCH; e = expr; WITH ; option(BAR); cl = separated_nonempty_list(BAR, case) {Parsed_ast.Match($sloc, e, cl)}
 | LET; i = LOWERCASE_IDENT; EQ; e1 = expr; IN; e2 = expr; {Parsed_ast.Let ($sloc, i, e1, e2)}
 | LET; REC; i = LOWERCASE_IDENT; EQ; e1 = expr; IN; e2 = expr; {Parsed_ast.LetRec ($sloc, i, e1, e2)}
 | DO; LCURLY; es = tuple_sep(SEMICOLON, expr); RCURLY { Parsed_ast.Seq($sloc, es)}

pattern2:
  | i = INT { Parsed_ast.Pat_Int ($sloc, i) }
  | i = LOWERCASE_IDENT {Parsed_ast.Pat_Ident ($sloc, i)}
  | i = UPPERCASE_IDENT {Parsed_ast.Pat_Constr ($sloc, i, None)}
  | EMPTY_LIST {Parsed_ast.Pat_Constr ($sloc, "Nil$", None)}
  | TRUE { Parsed_ast.Pat_Bool ($sloc, true) }
  | FALSE { Parsed_ast.Pat_Bool ($sloc, false)}
  | UNIT { Parsed_ast.Pat_Unit $sloc }
  | UNDERSCORE {Parsed_ast.Pat_Any $sloc}
  | LPAREN; pats = tuple_sep(COMMA, pattern2); RPAREN {Parsed_ast.Pat_Tuple ($sloc, pats)}
  | cname = UPPERCASE_IDENT; p = pattern2 {Parsed_ast.Pat_Constr ($sloc, cname, Some p)}
  | LPAREN; pat = pattern; RPAREN { pat }
  | LSQUARE; pat = pattern; RSQUARE {
    Parsed_ast.Pat_Constr ($sloc, "Cons$", Some (
      Parsed_ast.Pat_Tuple ($sloc, [pat; Parsed_ast.Pat_Constr($sloc, "Nil$", None)])
    ))
  }

pattern1:
  | p = pattern2 { p }
  | p0 = pattern2; CONS; p1 = pattern1 {
    Parsed_ast.Pat_Constr ($sloc, "Cons$", Some (
      Parsed_ast.Pat_Tuple ($sloc, [p0; p1])
    ))

    }

pattern:
  | p = pattern1 { p }
  | pats = tuple_sep(BAR, pattern1) { Parsed_ast.Pat_Or ($sloc, pats)}

case:
  (* removes ambiguity of nested matches *)
  | p = pattern; ARROW; e = expr2 {(p, e)}

decl:
  | TYPE; tparams = loption(type_params); tname = LOWERCASE_IDENT; EQ; option(BAR); cl = separated_nonempty_list(BAR, type_constr) {Parsed_ast.Type($sloc, tparams, tname, cl)}
  | VAL; vname = LOWERCASE_IDENT; EQ; e = expr {Parsed_ast.Val ($sloc, vname, e)}
  | VAL; REC; vname = LOWERCASE_IDENT; EQ; e = expr {Parsed_ast.ValRec ($sloc, vname, e)}

type_constr:
  | tconstr = UPPERCASE_IDENT; {Parsed_ast.DeclConstr ($sloc, tconstr, None) }
  | tconstr = UPPERCASE_IDENT; OF; texpr = type_expr { Parsed_ast.DeclConstr ($sloc, tconstr, Some(texpr))}
  (* special case to avoid parenthesis for tuple type *)
  | tconstr = UPPERCASE_IDENT; OF; texprs = tuple_sep(MUL, type_expr) { Parsed_ast.DeclConstr ($sloc, tconstr, Some(TyTuple($sloc, texprs)))}

(* do not require parenthesis when only one type parameter present *)

type_param:
  | APOSTROPHE; tparam = LOWERCASE_IDENT { ("'" ^ tparam) }

type_params:
  | tp = type_param { [tp] }
  | LPAREN; tps = tuple_sep(COMMA, type_param) RPAREN { tps }

(* two levels handles int -> int tree ambiguity *)

type_expr2:
  | TINT { Parsed_ast.TyInt ($sloc) }
  | TBOOL { Parsed_ast.TyBool ($sloc) }
  | TUNIT { Parsed_ast.TyUnit ($sloc) }
  | tparam = type_param { Parsed_ast.TyVar ($sloc, tparam)}
  | tname = LOWERCASE_IDENT { Parsed_ast.TyCustom ($sloc, [], tname) }
  | texpr = type_expr2; tname = LOWERCASE_IDENT { Parsed_ast.TyCustom ($sloc, [texpr], tname) }
  | LPAREN; texprs = tuple_sep(COMMA, type_expr); RPAREN; tname = LOWERCASE_IDENT {Parsed_ast.TyCustom ($sloc, texprs, tname) }
  | LPAREN; texprs = tuple_sep(MUL, type_expr); RPAREN { Parsed_ast.TyTuple ($sloc, texprs) }
  | LPAREN; texpr = type_expr; RPAREN { texpr }

type_expr:
  | texpr = type_expr2 { texpr }
  | t1 = type_expr; ARROW; t2 = type_expr {Parsed_ast.TyFun ($sloc, t1, t2)}