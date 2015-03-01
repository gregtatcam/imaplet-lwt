%{
(*open Core.Std let _ = _squelch_unused_module_warning_*)

%}

/*(* Standard tokens *)*/
%token <string> ERROR
%token EOF

/*(* Headers *)*/
%token <Field_name.t * string> FIELD
%token HEADER_END

/*(* Body *)*/
%token <string> OCTET_STREAM
%token <int> OCTET_STREAM_OFFSET

%token <string> STRING
%token <string> ATOM

%token EQUALS
%token SLASH
%token SEMICOLON


%start message
%type <Grammar_types.message> message

%start only_header
%type <Grammar_types.header> only_header

/*(* Content-type field *)*/
%start content_type
%type <Grammar_types.content_type> content_type

%%

message : part EOF { $1 };

part : header HEADER_END content 
  { 
    `Message ($1, $3)
  }
| header
  {
    `Message ($1, `Truncated)
  }
  ;
only_header : header EOF { $1 };

header :
  { [] }
  | header FIELD
    { 
      let header = $1 in
      let name_body = $2 in
      (name_body :: header)
    }
;

content : OCTET_STREAM_OFFSET  { `Content_offset $1 } ;
/*
(*
  | part_list CLOSE_BOUNDARY { `Content (None, Some $1) }
;

part_list :
    { [] }
  | part_list OPEN_BOUNDARY part { $2 :: $3 }
  *)
*/
content_type : ctype SLASH csubtype param_list { ($1, $3, $4) }
;

ctype : ATOM { $1 };
csubtype : ATOM { $1 };

/*(* Some implementations wrongfully add semicolons at the end of the Content-type field.
This rule allows for it.
*)*/
param_list : 
  | param_list_aux EOF { $1 }
  | param_list_aux semicolon EOF { $1 }
;

/*(* Be tolerant of repeated semicolons *)*/
semicolon :
  | SEMICOLON { () }
  | semicolon SEMICOLON { () }
;

param_list_aux :
    { [] }
  | param_list_aux semicolon parameter { $3 :: $1 }
;

parameter : attribute EQUALS value { ($1, $3) };
attribute : ATOM { $1 };
value : 
    ATOM { $1 }
  | STRING { $1 }
;

%%


