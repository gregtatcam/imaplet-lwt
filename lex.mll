(*
 * Copyright (c) 2013-2014 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
{
open Core.Std
open Lexing
open Parser
open Printf

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let debug frmt = printf frmt (*(fun format a -> ())*)

let print_first f = if !f = true then printf "first is true\n" else printf "first is false\n"

let match_date str =
let group re = "\\(" ^ re ^ "\\)" in
let orx re1 re2 = re1 ^ "\\|" ^ re2 in
let mon = group "Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec" in
let dd = group ( orx ( group " [0-9]") (group "[0-9][0-9]")) in
let yyyy = group "[0-9][0-9][0-9][0-9]" in
let time = group "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]" in
let zone = group "[+-][0-9][0-9][0-9][0-9]" in
let date = group ("\"" ^ dd ^ "-" ^ mon ^ "-" ^ yyyy ^ " " ^ time ^ " " ^ zone ^ "\"") in
  try
    (Str.search_forward (Str.regexp date) str 0) >= 0
  with _ -> false


let flags_table = String.Table.create()
    let _ = List.iter 
  [
   "Deleted"            ,FLDELETED;
   "deleted"            ,FLDELETED;
   "Answered"           ,FLANSWERED;
   "answered"           ,FLANSWERED;
   "Draft"              ,FLDRAFT;
   "draft"              ,FLDRAFT;
   "Flagged"            ,FLFLAGGED;
   "flagged"            ,FLFLAGGED;
   "Seen"               ,FLSEEN;
   "seen"               ,FLSEEN;
  ] (fun (kwd, tok) -> Hashtbl.add_exn flags_table kwd tok)


let keyword_table = String.Table.create()
    let _ = List.iter 
 [ "ALL"		,ALL;
   "ANSWERED"		,ANSWERED;
   "APPEND"		,APPEND;
   "LAPPEND"		,LAPPEND;
   "AUTHENTICATE"	,AUTHENTICATE;
   "EXAMINE"		,EXAMINE;
   "EXPUNGE"		,EXPUNGE;
   "EXTERNAL"		,EXTERNAL;
   "BCC"		,BCC;
   "BEFORE"		,BEFORE;
   "BODY"	        ,BODY;
   "BODYSTRUCTURE"	,BODYSTRUCTURE;
   "CAPABILITY"		,CAPABILITY;
   "CC"			,CC;
   "CHANGEDSINCE"	,CHANGEDSINCE;
   "CHARSET"		,CHARSET;
   "CHECK"		,CHECK;
   "CLOSE"		,CLOSE;
   "COPY"		,COPY;
   "CREATE"		,CREATE;
   "DELETE"		,DELETE;
   "DELETED"		,DELETED;
   "DRAFT"		,DRAFT;
   "ENVELOPE"		,ENVELOPE;
   "FAST"		,FAST;
   "FETCH"		,FETCH;
   "FLAGGED"		,FLAGGED;
   "FLAGS"		,FLAGS;
   "-FLAGS"		,FLAGSMIN;
   "+FLAGS"		,FLAGSPL;
   "FLAGS.SILENT"	,FLAGSSILENT;
   "-FLAGS.SILENT"	,FLAGSSILENTMIN;
   "+FLAGS.SILENT"	,FLAGSSILENTPL;
   "FROM"		,FROM;
   "FULL"		,FULL;
   "GSSAPI"		,GSSAPI;
   "HEADER"		,HEADER;
   "INTERNALDATE"	,INTERNALDATE;
   "ID"			,ID;
   "IDLE"		,IDLE;
   "INBOX"		,INBOX;
   "KERBEROS_V4"	,KERBEROS_V4;
   "KEYWORD"		,KEYWORD;
   "LARGER"		,LARGER;
   "LIST"		,LIST;
   "LSUB"		,LSUB;
   "LOGIN"		,LOGIN;
   "LOGOUT"		,LOGOUT;
   "MESSAGES"		,MESSAGES;
   "MODSEQ"		,MODSEQ;
   "NEW"		,NEW;
   "NOT"		,NOT;
   "NOOP"		,NOOP;
   "OLD"		,OLD;
   "ON"			,ON;
   "OR"			,OR;
   "PLAIN"		,PLAIN;
   "PRIV"		,PRIV;
   "RECENT"		,RECENT;
   "RENAME"		,RENAME;
   "RFC822"		,RFC822;
   "RFC822.HEADER"	,RFC822HEADER;
   "RFC822.SIZE"	,RFC822SIZE;
   "RFC822.TEXT"	,RFC822TEXT;
   "SEARCH"		,SEARCH;
   "SEEN"		,SEEN;
   "SELECT"		,SELECT;
   "SENTBEFORE"		,SENTBEFORE;
   "SENTON"		,SENTON;
   "SENTSINCE"		,SENTSINCE;
   "SINCE"		,SINCE;
   "SKEY"		,SKEY;
   "SMALLER"		,SMALLER;
   "STARTTLS"		,STARTTLS;
   "STATUS"		,STATUS;
   "STORE"		,STORE;
   "SUBJECT"		,SUBJECT;
   "TEXT"		,TEXT;
   "TO"			,TO;
   "SHARED"		,SHARED;
   "SUBSCRIBE"		,SUBSCRIBE;
   "UID"		,UID;
   "UIDNEXT"		,UIDNEXT;
   "UIDVALIDITY"	,UIDVALIDITY;
   "UNANSWERED"		,UNANSWERED;
   "UNCHANGEDSINCE"	,UNCHANGEDSINCE;
   "UNDELETED"		,UNDELETED;
   "UNDRAFT"		,UNDRAFT;
   "UNFLAGGED"		,UNFLAGGED;
   "UNKEYWORD"		,UNKEYWORD;
   "UNSUBSCRIBE"	,UNSUBSCRIBE;
   "UNSEEN"		,UNSEEN  ] (fun (kwd, tok) -> Hashtbl.add_exn keyword_table kwd tok)
}

let number = ['0'-'9']+
let nz_number = ['1'-'9'] ['0'-'9']*
let crlf = "\r\n"
let quoted_chars = ([^'\r' '\n' '\"' '\\'] | ('\\' '\\') | ('\\' '\"'))*
let atom_chars =    ([^'(' ')' '{' ' ' '%' '*' '\"' '\\' ']' '\r' '\n'])+
let astring_chars = ([^'(' ')' '{' ' ' '%' '*' '\"' '\\' ']' '\r' '\n'] | ']')+
let tag =           ([^'(' ')' '{' ' ' '%' '*' '\"' '\\' ']' '\r' '\n' '+'] | ']')+ 
let anychar =       ([^'\n' '\r' ' ' '\"' '\\' '(' ')' '{' ']'])+
let done = ['d' 'D'] ['o' 'O'] ['n' 'N'] ['e' 'E'] ['\r'] ['\n']
let body = (['b' 'B'] ['o' 'O'] ['d' 'D'] ['y' 'Y'] '[' [^']']* ']' 
  (['<'] (number ('.' nz_number)?)? ['>'])?)
let bodypeek = (['b' 'B'] ['o' 'O'] ['d' 'D'] ['y' 'Y'] '.' ['p' 'P'] ['e' 'E'] ['e' 'E'] ['k' 'K'] '[' [^']']*']'
  (['<'] (number ('.' nz_number)?)?['>'])?)
  
rule read context =
  parse
  | '\r' '\n'  			{ debug "l:CRLF %d\n%!" 1; context := `Tag; CRLF }
  | eof      			{ debug "l:EOF\n%!"; EOF }
  | body as b                   { debug "l:body %s\n%!" b; BODYFETCH(b) }
  | bodypeek as b               { debug "l:body.peek %s\n%!" b; BODYPEEK(b) }
  | '('				{ debug "l:(\n%!"; LP}
  | ')'				{ debug "l:)\n%!"; RP}
  | ' '    			{ debug "l:SP\n%!"; SP }
  | '{' (['0'-'9']+ as n) '+' '}'   { debug "l:literal-plus %s\n%!" n; LITERALPL(int_of_string(n)) }
  | '{' (['0'-'9']+ as n) '}'   { debug "l:literal %s\n%!" n; LITERAL(int_of_string(n)) }
  | '\"' quoted_chars '\"' as qs
                                { debug "l:qs %s\n%!" qs; 
                                  if match_date qs then
                                    DATE(qs)
                                  else
                                    QUOTED_STRING (qs) }
  | done                        { debug "l:DONE\n%!"; DONE }
  | anychar as cmd              { debug "l:maybe cmd %s\n%!" cmd;
                                if !context <> `Tag then (
                                  try 
                                    Hashtbl.find_exn keyword_table (String.uppercase cmd) 
                                  with Not_found -> 
                                    debug "l:command not found %s\n%!" cmd; 
                                    ATOM_CHARS (cmd)
                                ) else (
                                  debug "l:tag %s\n" cmd; context := `Any; TAG (cmd)
                                )
                                }
  | '\\' (atom_chars as fl)       { debug "l:flag %s\n%!" fl; try Hashtbl.find_exn flags_table fl
                                  with Not_found -> 
                                    FLEXTENSION (fl)
                                }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
