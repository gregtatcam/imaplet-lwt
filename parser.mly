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
%token ALL
%token ANSWERED
%token APPEND
%token LAPPEND
%token <string> ATOM_CHARS
%token AUTHENTICATE
%token EXAMINE
%token EXPUNGE
%token BCC
%token BEFORE
%token BODY
%token <string> BODYFETCH
%token <string> BODYPEEK
%token BODYSTRUCTURE
%token CAPABILITY
%token CC
%token CHARSET
%token CHECK
%token CLOSE
%token COPY
%token CREATE
%token CRLF
%token <string>DATE
%token DELETE
%token DELETED
%token DONE
%token DRAFT
%token ENVELOPE
%token EOF
%token EXTERNAL
%token FAST
%token FETCH
%token FLAGGED
%token FLAGS
%token FLAGSPL
%token FLAGSMIN
%token FLAGSSILENT
%token FLAGSSILENTPL
%token FLAGSSILENTMIN
%token FLANSWERED
%token FLFLAGGED
%token FLDELETED
%token FLSEEN
%token FLDRAFT
%token <string>FLEXTENSION
%token FROM
%token FULL
%token GSSAPI
%token HEADER
%token ID
%token IDLE
%token INBOX
%token INTERNALDATE
%token KEYWORD
%token KERBEROS_V4
%token LARGER
%token LP
%token <int> LITERAL
%token <int> LITERALPL
%token LIST
%token LSUB
%token LOGIN
%token LOGOUT
%token MESSAGES
%token NEW
%token NOT
%token NOOP
%token OLD
%token ON
%token OR
%token PLAIN
%token <string> QUOTED_STRING
%token RECENT
%token RENAME
%token RFC822
%token RFC822HEADER
%token RFC822SIZE
%token RFC822TEXT
%token RP
%token SEARCH
%token SEEN
%token SELECT
%token SENTBEFORE
%token SENTON
%token SENTSINCE
%token SINCE
%token SKEY
%token SMALLER
%token SP
%token STARTTLS
%token STATUS
%token STORE
%token SUBJECT
%token <string> TAG
%token TEXT
%token TO
%token SUBSCRIBE
%token UID
%token UIDNEXT
%token UIDVALIDITY
%token UNANSWERED
%token UNDELETED
%token UNDRAFT
%token UNFLAGGED
%token UNKEYWORD
%token UNSUBSCRIBE
%token UNSEEN

%{
open Imaplet_types
let debug format = 
  let open Batteries in
  BatLog.Easy.logf `debug format (*(fun format a -> ())*)
%}

%start <Imaplet_types.clientRequest> request
%%

request:
  | EOF	{ debug "got end of file\n%!"; ("",Done) }
  | cmd = command { debug "p:request\n%!"; (cmd) }

list_of_quoted_string:
  | lqs = separated_list(SP, quoted) { lqs } 

(**
nstring:
  | s = string_ { s }
  | NIL    { "NIL" }
**)

quoted:
  | qs = QUOTED_STRING { qs }

command:
  | t = tag_; SP; c = commands; CRLF { debug "p:command\n%!"; (t,c) }
  | c = c_done { ("",c) }

tag_:
  | s = TAG { s }

commands:
  | c = command_any {Any(c)}
  | c = command_auth {Authenticated(c)}
  | c = command_notauth {Notauthenticated(c)}
  | c = command_select {debug "p:commands\n%!"; Selected(c)}

command_any:
  | CAPABILITY { Cmd_Capability }
  | LOGOUT { Cmd_Logout }
  | NOOP { Cmd_Noop }
  | i = id { i }

id:
  | ID; SP; l = delimited(LP, list_of_quoted_string, RP) { Cmd_Id l }

command_auth:
  | c = c_append { c }
  | c = c_create { c }
  | c = c_delete { c }
  | c = c_examine { c }
  | c = c_idle { debug "p:idle command\n%!"; c }
  | c = c_list  { c }
  | c = c_lsub { c }
  | c = c_rename { c }
  | c = c_select { c }
  | c = c_status { c }
  | c = c_subscribe { c }
  | c = c_unsubscribe { c }

(** list option and date/time are not defined TBD **)
c_append:
  | m= append_cmd; SP; l = flag_list_sp; d = date_sp; n = literal { Cmd_Append (m, l, d, n) }

append_cmd:
  | APPEND; SP; m = mailbox {debug "p:append_cmd %s\n%!" m; (m)}

c_lappend:
  | LAPPEND; SP; u = user; SP; m = mailbox; SP; l = literal 
    {debug "p:c_lappend %s %s\n%!" u m; Cmd_Lappend (u,m,l)}

literal:
  | n = LITERAL {debug "p:literal %d\n%!" n;Literal(n)}
  | n = LITERALPL {debug "p:literal plus %d\n%!" n;LiteralPlus(n)}

c_create:
  | CREATE; SP; m = mailbox { Cmd_Create m }

c_delete:
  | DELETE; SP; m = mailbox { Cmd_Delete m }

c_idle:
  | IDLE { Cmd_Idle }

c_done:
  | DONE { Authenticated(Cmd_Done) }

c_examine:
  | EXAMINE; SP; m = mailbox { Cmd_Examine m }

(** second argument should be list-mailbox **)
c_list:
  | LIST; SP; m = mailbox ; SP; m1 = mailbox { Cmd_List (m, m1)}

c_lsub:
  | LSUB; SP; m = mailbox; SP; m1 = mailbox { Cmd_Lsub (m, m1)}

c_rename:
  | RENAME; SP; m = mailbox; SP; m1 = mailbox { Cmd_Rename (m, m1)}

c_select:
  | SELECT; SP; m = mailbox { Cmd_Select m }

c_status:
  | STATUS; SP; m = mailbox ; SP; l = status_list { Cmd_Status (m, l)}

mailbox:
  | INBOX { "INBOX" }
  | s = ATOM_CHARS { debug "p:mailbox\n%!"; s }
  | s = quoted {Regex.dequote s}

status_list:
  | l = delimited(LP, separated_list(SP, status_att), RP) { l }

status_att:
  | MESSAGES { Stat_Messages }
  | RECENT { Stat_Recent }
  | UIDNEXT { Stat_Uidnext }
  | UIDVALIDITY { Stat_Uidvalidity }
  | UNSEEN { Stat_Unseen }

c_subscribe:
  | SUBSCRIBE; SP; m = mailbox { Cmd_Subscribe m }

c_unsubscribe:
  | UNSUBSCRIBE; SP; m = mailbox { Cmd_Unsubscribe m }

command_notauth:
  | c = login { c }
  | c = authenticate { c }
  | c = c_lappend { c }
  | STARTTLS { Cmd_Starttls }

login:
  | LOGIN; SP; u = user; SP; p = password { Cmd_Login (u, p) }

user:
  | a = astring { a }

password:
  | a = astring { a }

astring:
  | s = ATOM_CHARS { debug "p:astring\n%!"; s }
  | q = quoted { q }

(** needs more definition, if plain then can be followed by the base64 **)
authenticate:
  |AUTHENTICATE; SP; a = auth_type; SP; s=ATOM_CHARS { debug "p:authenticate command\n%!"; Cmd_Authenticate (a,s) } (** if not plain then CRLF base64 **)

auth_type:
  | KERBEROS_V4 { Auth_Kerberos_v4 }
  | GSSAPI { Auth_Gssapi }
  | SKEY { Auth_Skey }
  | EXTERNAL { Auth_External }
  | PLAIN { debug "p:plain authentication\n%!"; Auth_Plain }

command_select:
  | CHECK { Cmd_Check }
  | CLOSE { Cmd_Close }
  | EXPUNGE { Cmd_Expunge }
  | c = c_search { debug "p:command_select\n%!"; c }
  | c = c_fetch { c }
  | c = c_store { c }
  | c = c_copy { c }

c_copy:
  | COPY; SP; seq = ATOM_CHARS; SP; m = mailbox {Cmd_Copy(Interpreter.get_sequence seq,m,false) }
  | UID; SP; COPY; SP; seq = ATOM_CHARS; SP; m = mailbox {Cmd_Copy(Interpreter.get_sequence seq,m,true) }

c_store:
  | STORE; SP; seq = ATOM_CHARS; SP; f = s_flags; SP; v = s_flags_value { debug "p:store\n%!"; Cmd_Store (Interpreter.get_sequence seq,f,v,false) }
  | UID; SP; STORE; SP; seq = ATOM_CHARS; SP; f = s_flags; SP; v = s_flags_value { debug "p:store\n%!"; Cmd_Store (Interpreter.get_sequence seq,f,v, true) }

s_flags_value:
  | l = flag_list {l}
  | l = flag_sp_list {l}

s_flags:
  | FLAGS {Store_Flags}
  | FLAGSSILENT {Store_FlagsSilent}
  | FLAGSPL {Store_PlusFlags}
  | FLAGSSILENTPL {Store_PlusFlagsSilent}
  | FLAGSMIN {Store_MinusFlags}
  | FLAGSSILENTMIN {Store_MinusFlagsSilent}

c_fetch:
  | FETCH; SP; seq = ATOM_CHARS; SP; a = fetch_args {Cmd_Fetch(Interpreter.get_sequence seq,a,false)}
  | UID; SP; FETCH; SP; seq = ATOM_CHARS; SP; a = fetch_args {Cmd_Fetch(Interpreter.get_sequence seq,a,true)}

fetch_args:
  | s = fetch_macro {FetchMacro(s)}
  | s = fetch_att_list {s}

fetch_macro:
  | ALL {Fetch_All}
  | FAST {Fetch_Fast}
  | FULL {Fetch_Full}

fetch_att_list:
  | s = fetch_att {FetchAtt([s])}
  | s = delimited(LP, separated_list(SP, fetch_att), RP) { debug "p:fetch-att list\n%!"; FetchAtt(s) }

fetch_att:
  | ENVELOPE {Fetch_Envelope}
  | FLAGS {Fetch_Flags}
  | INTERNALDATE {Fetch_Internaldate}
  | RFC822 {Fetch_Rfc822}
  | RFC822HEADER {Fetch_Rfc822Header}
  | RFC822SIZE {Fetch_Rfc822Size}
  | RFC822TEXT {Fetch_Rfc822Text}
  | BODY {Fetch_Body}
  | s = BODYFETCH; {debug "p:fetch_att body %s\n%!" s; let (sec,parts) =
    Fetchregex.parse_fetch_section s in Fetch_BodySection(sec,parts)}
  | s = BODYPEEK; {debug "p:fetch_att body %s\n%!" s; let (sec,parts) =
    Fetchregex.parse_fetch_section s in Fetch_BodyPeekSection(sec,parts)}
  | BODYSTRUCTURE {Fetch_Bodystructure}
  | UID {Fetch_Uid}

c_search:
  | SEARCH; SP; c = charset_sp; l = search_keys {debug "p:c_search\n%!"; Cmd_Search (c,l, false)}
  | UID; SP; SEARCH; SP; c = charset_sp; l = search_keys {debug "p:c_search\n%!"; Cmd_Search(c,l, true)}

charset_sp:
  | {None}
  | c = charset; SP {Some c}

charset:
  | CHARSET; SP; a = ATOM_CHARS {a}

search_keys:
 | s = separated_list(SP, op_search_keys) {debug "p:list_search_keys\n%!"; KeyList(s)}

op_search_keys:
  | s = op_search_key {debug "p:key\n%!"; s}
  | s = delimited(LP, separated_list(SP, op_search_keys), RP) { debug "p:list key\n%!"; KeyList(s) }
  | NOT; SP; s = op_search_keys {debug "p:not key\n%!"; NotKey(s)}
  | OR; SP; s1 = op_search_keys; SP; s2 = op_search_keys {debug "p:or key\n%!"; OrKey(s1,s2)}

op_search_key:
  | s = search_key {debug "p:op_search_key\n%!"; Key s}

search_key:
  | ALL {Search_All}
  | ANSWERED {Search_Answered}
  | BCC; SP; s = ATOM_CHARS {Search_Bcc (s)}
  | BEFORE; SP; s = s_date {Search_Before ( s)}
  | BODY; SP; s= ATOM_CHARS; {Search_Body (s)}
  | CC; SP; s = ATOM_CHARS; {Search_Cc (s)}
  | DELETED {Search_Deleted}
  | FLAGGED {Search_Flagged} 
  | FROM; SP; s = ATOM_CHARS {Search_From (s) }
  | KEYWORD; s = ATOM_CHARS {Search_Keyword (s) } (** flag-keyword **) 
  | NEW {Search_New}
  | OLD {Search_Old} 
  | ON; SP; s= s_date {Search_On(s)}
  | RECENT {Search_Recent} 
  | SEEN {Search_Seen} 
  | SINCE; SP; s = s_date {Search_Since(s)}
  | SUBJECT; SP; s = ATOM_CHARS {Search_Subject (s)}
  | TEXT; SP; s = ATOM_CHARS {Search_Text (s) }
  | TO; SP; s = ATOM_CHARS {Search_To (s) }
  | UNANSWERED {Search_Unanswered}
  | UNDELETED {Search_Undeleted}
  | UNFLAGGED {Search_Unflagged} 
  | UNKEYWORD; SP; s = ATOM_CHARS {Search_Unkeyword (s) }
  | UNSEEN {Search_Unseen} 
  (** above this line were in IMAP2 **)
  | DRAFT {Search_Draft} 
  | h = s_header {h}
  | LARGER; SP; n = ATOM_CHARS {Search_Larger (int_of_string n)}
  | SENTBEFORE; SP; s = s_date {Search_Sentbefore (s)}
  | SENTON; SP; d = s_date {debug "p:s_senton\n%!"; Search_Senton(d)}
  | SENTSINCE; SP; s = s_date {Search_Sentsince (s)}
  | SMALLER; SP; n = ATOM_CHARS {Search_Smaller(int_of_string n)}
  | UID; SP; s = ATOM_CHARS {Search_UID (Interpreter.get_sequence s)} 
  | UNDRAFT {Search_Undraft} 
  | s = ATOM_CHARS {Search_SeqSet (Interpreter.get_sequence s)} 

s_header:
  | HEADER; SP; SUBJECT; SP; s2 = ATOM_CHARS {Search_Header ("SUBJECT",s2)} (** header-fld-name **)
  | HEADER; SP; s1=ATOM_CHARS; SP; s2 = ATOM_CHARS {Search_Header (s1,s2)} (** header-fld-name **)

s_date:
  | q = quoted {debug "p:s_date %s\n%!" q; Dates.imapd_to_date_exn q}
  | s = ATOM_CHARS {debug "p:s_date %s\n%!" s; Dates.imapd_to_date_exn s}

flag_list_sp:
  | {None}
  | l = flag_list; SP {Some l}

flag_list:
  | l = delimited(LP, flag_sp_list, RP) { debug "p:flag_list\n%!"; l }

flag_sp_list:
  | l = separated_list(SP, flag) { debug "p:flag_sp_list\n%!"; l }

flag:
  | FLANSWERED {debug "p:flag \n%!"; Flags_Answered}
  | FLFLAGGED {debug "p:flag \n%!"; Flags_Flagged}
  | FLDELETED {debug "p:flag \n%!"; Flags_Deleted}
  | FLSEEN {debug "p:flag \n%!"; Flags_Seen}
  | FLDRAFT {debug "p:flag \n%!"; Flags_Draft}
  | f = FLEXTENSION {debug "p:flag \n%!"; Flags_Extention (f) }
  | f = ATOM_CHARS {debug "p:flag \n%!"; Flags_Keyword (f) }

date_sp:
  | {None}
  | d = date; SP {debug "p:sp_date %s\n%!" d; Some (Dates.imapd_to_date_time_exn d)}

date:
  | d=DATE {debug "p:date %s\n%!" d; d}
