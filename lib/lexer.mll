rule tokenize = parse
| "{{" { Parser.OPEN }
| "::" { Parser.SEP }
| "}}" { Parser.CLOSE }
| [^'{' ':' '}']+ as s { Parser.TEXT s }
| _ as c { Parser.TEXT (String.make 1 c) }
| eof { Parser.EOF }
