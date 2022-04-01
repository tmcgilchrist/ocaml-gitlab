{
open Parser

exception Error of string
}

let whitespace = ['\t' ' ']*
let eol = '\n' | "\r\n"
let key = ['A'-'Z''a'-'z''0'-'9''_''-']+
let value = [^'\n''\r']+
let section_name = [^'['']']+

rule token = parse
| whitespace '[' (section_name as name) ']' whitespace eol { SECTIONHEADER name }
| whitespace (key as key) whitespace '=' whitespace (value as value) eol { KEYVAL (key,value) }
| eof { EOF }
