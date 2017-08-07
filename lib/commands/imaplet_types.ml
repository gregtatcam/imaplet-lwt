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
open Sexplib
open Sexplib.Std

type mailboxFlags =
  | Flags_Answered
  | Flags_Deleted
  | Flags_Draft
  | Flags_Flagged
  | Flags_Recent
  | Flags_Seen
  | Flags_Keyword of string
  | Flags_Extention of string
  | Flags_Template
  [@@deriving sexp]

type literal = Literal of int | LiteralPlus of int

type seq_number = Wild | Number of int [@@deriving sexp]

type seq_set =
  | SeqNumber of seq_number
  | SeqRange of seq_number * seq_number [@@deriving sexp]

type sequence = seq_set list [@@deriving sexp]

type responseCode = 
  | RespCode_Alert
  | RespCode_Badcharset
  | RespCode_Capability
  | RespCode_Parse
  | RespCode_Permanentflags
  | RespCode_Read_only
  | RespCode_Read_write
  | RespCode_Trycreate
  | RespCode_Uidnext
  | RespCode_Uidvalidity
  | RespCode_Unseen
  | RespCode_Highestmodseq

type state = 
 | State_Notauthenticated
 | State_Authenticated
 | State_Selected
 | State_Logout

type statusOpt = 
  | Stat_Highestmodseq
  | Stat_Messages
  | Stat_Recent
  | Stat_Uidnext
  | Stat_Uidvalidity
  | Stat_Unseen
  
type authtype = 
  | Auth_Kerberos_v4
  | Auth_Gssapi
  | Auth_Skey
  | Auth_External
  | Auth_Plain

type entryType =
  | Entry_Shared
  | Entry_Priv
  | Entry_All
  [@@deriving sexp]
  
type searchKey = 
  | Search_All (** all messages in the mailbox; the default initial key for ANDing **)
  | Search_Answered (** messages with the \Answered flag set **)
  | Search_Bcc of string (** messages with string in the envelope sructure's BCC field **) 
  | Search_Before of Dates.ImapDate.t (** messages with internal date **) 
  | Search_Body of string (** messages containing string in teh body **) 
  | Search_Cc of string (** messages with string int the envelope structure's CC field **)
  | Search_Deleted (** with \Deleted flag set **) 
  | Search_Draft (** with \Draft flag set **)
  | Search_Flagged (** with \Flagged flag set **)
  | Search_From of string (** messages with string in the envelope structure's FROM field **)
  | Search_Header of string * string (** messages with the header with the specified filed name * specified string **)
  | Search_Keyword of string (** messages with the keyword flag set **)
  | Search_Larger of int (** messages with the size larger than specified **) 
  | Search_New (** messages with \Recent set but not \Seen **)
  | Search_Modseq of (string * entryType) option * int64 (** modseq higher than for specific metadata **)
  | Search_Old (** message with no \Recent flag set **) 
  | Search_On of Dates.ImapDate.t (** messages with internal date within the specified date **)
  | Search_Recent (** messages with \Recent flag set **)
  | Search_Seen (** message with \Seen flag set **)
  | Search_Sentbefore of Dates.ImapDate.t (** messages with Date: header is earlier **)
  | Search_Senton of Dates.ImapDate.t (** messages with Date: header is within **)
  | Search_Sentsince of Dates.ImapDate.t (** messages with Date: header is within or later **)
  | Search_SeqSet of sequence (** messages with the sequence numbers **)
  | Search_Since of Dates.ImapDate.t (** messages with internal date within or later **)
  | Search_Smaller of int (** messages with size smaller **)
  | Search_Subject of string (** messages with envelope structure's SUBJECT field **)
  | Search_Text of string (** messages with the string in the header or body, could be literal **) 
  | Search_To of string (** messages with the envelope structure's TO field **)
  | Search_UID of sequence (** messages with unique identifier **)
  | Search_Unanswered (** messages with \Answered flag not set **)
  | Search_Undeleted (** messages with \Deleted flag not set **)
  | Search_Undraft (** messages with \Draft flag not set **)
  | Search_Unflagged (** messages with \Flagged flag not set **)
  | Search_Unkeyword of string (** message that do not have the specified keyword flag set **)
  | Search_Unseen (** messages with \Seen flag not set **) 
  [@@deriving sexp]

type 'a searchKeys =
  | Key of 'a
  | KeyList of 'a searchKeys list
  | OrKey of 'a searchKeys * 'a searchKeys
  | NotKey of 'a searchKeys 
  [@@deriving sexp]
  
type fetchMacro = 
  | Fetch_All
  | Fetch_Fast
  | Fetch_Full
  
type sectionMsgtext =
  | Header
  | HeaderFields of string list
  | HeaderFieldsNot of string list
  | Text

type sectionPart = int list

type sectionText =
  | SectionMsgtext of sectionMsgtext 
  | Mime

type sectionSpec = 
  | SectionMsgtext of sectionMsgtext option
  | SectionPart of sectionPart * (sectionText option)

type bodyPart = int list (** 0,1,2 **)

type fetchAtt =
  | Fetch_Body
  | Fetch_BodySection of sectionSpec * bodyPart
  | Fetch_BodyPeekSection of sectionSpec  * bodyPart
  | Fetch_Bodystructure 
  | Fetch_Envelope 
  | Fetch_Flags
  | Fetch_Internaldate
  | Fetch_Modseq
  | Fetch_Rfc822
  | Fetch_Rfc822Header 
  | Fetch_Rfc822Size
  | Fetch_Rfc822Text
  | Fetch_Uid

type fetch =
  | FetchMacro of fetchMacro
  | FetchAtt of fetchAtt list
  
type searchFlags =
  | Common of mailboxFlags
  | NotCommon of mailboxFlags
  | Old
  | New
  
type storeFlags = 
  | Store_Flags
  | Store_FlagsSilent
  | Store_PlusFlags
  | Store_PlusFlagsSilent
  | Store_MinusFlags
  | Store_MinusFlagsSilent

type compressionAlgrthm =
  | Compr_Deflate

type compression = compressionAlgrthm * Zlib.stream * unit Lwt.t * Lwt_io.input_channel
  
type anyCmd = 
  | Cmd_Enable of string
  | Cmd_Id of string list
  | Cmd_Capability
  | Cmd_Noop
  | Cmd_Logout (** close connection **)
  | Cmd_Compress of compressionAlgrthm 
  
type notAuthenticatedCmd =  
  | Cmd_Starttls (** start tls negotiation **)
  | Cmd_Authenticate of authtype * string option (** authentication mechanism **)
  | Cmd_Login of string * string (** user * password **)
  | Cmd_Lappend of string * string option * string * literal (** user * password * mailbox **)
  
type authenticatedCmd =  
  | Cmd_Select of string * bool (** mailbox name * condstore **) 
  | Cmd_Examine of string * bool (** mailbox name * condstore **) 
  | Cmd_Create of string (** mailbox name **)
  | Cmd_Delete of string (** mailbox name **)
  | Cmd_Rename of string * string (** existing mailbox name * new mailbox name **)
  | Cmd_Subscribe of string (** mailbox name **)
  | Cmd_Unsubscribe of string (** mailbox name **)
  | Cmd_List of string * string (** reference name * mailbox name with possible wildcards **)
  | Cmd_Lsub of string * string (** reference name * mailbox name with possible wildcards **)
  | Cmd_Status of string * (statusOpt list) (** mailbox name * status data itme names **)
  | Cmd_Append of string * mailboxFlags list option * Dates.ImapTime.t option * literal (** mailbox name * optional flag parenthesized list * optional date/time string; message literal **)
  | Cmd_Idle
  | Cmd_Done

type selectedCmd =  
  | Cmd_Check (** request a checkpoint - housekeeping, implementation dependant **)
  | Cmd_Close (** transition to authenticated state **)
  | Cmd_Expunge (** permanently remove all messages with \Deleted flag **)
  | Cmd_Search of string option * (searchKey) searchKeys * bool (** optional charset * searching criteria; charset and criteria need more grammar definition TBD **)
  | Cmd_Fetch of sequence *  fetch * int64 option * bool (** more work is needed TBD **)
  | Cmd_Store of sequence * storeFlags * mailboxFlags list * int64 option * bool 
  | Cmd_Copy of sequence * string * bool (** sequence * mailbox name **)

type fromClient = 
  | Any of anyCmd
  | Notauthenticated of notAuthenticatedCmd
  | Authenticated of authenticatedCmd
  | Selected of selectedCmd
  | Done

type clientRequest =
  {tag:string; command:fromClient} 

let is_compress command =
  match command.command with
  | Any cmd -> (match cmd with |Cmd_Compress c -> Some c |_-> None)
  | _ -> None

let is_idle command =
  match command.command with
  | Authenticated cmd -> (match cmd with |Cmd_Idle -> true |_->false)
  | _ -> false

let is_done command =
  match command.command with
  | Authenticated cmd -> (match cmd with |Cmd_Done -> true |_->false)
  | _ -> false

type response = 
 | Resp_Ok of responseCode option * string
 | Resp_Bad of responseCode option * string
 | Resp_No of responseCode option * string
 | Resp_Bye of responseCode option * string
 | Resp_Preauth of responseCode option * string
 | Resp_Cont of string 
 | Resp_Untagged of string
 | Resp_Any of string
