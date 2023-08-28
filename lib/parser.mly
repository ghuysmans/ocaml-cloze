%token OPEN SEP CLOSE EOF
%token <string> TEXT
%start <[`With_mask] Ast.t> main
%%
t: s=TEXT { Ast.Str s }
| OPEN m=TEXT SEP l=t* h=preceded(SEP, TEXT)? CLOSE { Ast.(Mask (m, cat l, h)) }
main: l=t* EOF { Ast.cat l }
