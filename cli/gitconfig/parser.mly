%{
%}

%token <Types.binding> KEYVAL
%token <string> SECTIONHEADER
%token EOF

%start config

%type <Types.config> config
%type <Types.binding> key_value

%%
config:
| section* EOF { $1 }

section:
| section_header key_value* { $1, $2 }

section_header:
| SECTIONHEADER { $1 }

key_value:
| KEYVAL { $1 }
