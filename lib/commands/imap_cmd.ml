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
open Lwt
open Imaplet_types
open Response
open Regex
open Context
open Utils

exception SystemError of string
exception ExpectedDone
exception ClientTimedOut

let response context state resp mailbox =
  begin
  match state with
  |None -> ()
  |Some state -> 
    context.state := state
  end;
  begin
  match mailbox with
  |None -> ()
  |Some mailbox -> context.mailbox := mailbox
  end;
  return resp

let examine context =
  match Amailbox.selected_mbox context.!mailbox with
  | Some mailbox -> Amailbox.examine context.!mailbox mailbox
  | None -> return `NotExists

let resp_highestmodseq is_modseq context () =
  if is_modseq then
    match context.!highestmodseq with
    | `Sessionstart ->
      begin
      examine context >>= function
      |`Ok(_,status) -> 
         context.highestmodseq := `Highestmodseq;
         write_resp context.id context.!netw (Resp_Ok (Some RespCode_Highestmodseq, Int64.to_string status.modseq))
      |_ -> return ()
      end
    | _ -> return ()
  else
    return ()

let is_client_id l name value =
  List.exists (fun (n,v) -> 
    Regex.match_regex ~regx:("^\"?" ^ name) n && Regex.match_regex ~case:false ~regx:value v
  ) l

(* handle all commands
 * return either Ok, Bad, No, Preauth, Bye, or Continue response
 *)

(*
 * Any state
 *)

let selected_mailbox_exn mbox =
  option_value_exn (Amailbox.selected_mbox mbox)

let get_selected context =
  match Amailbox.user context.!mailbox with
  | Some user -> 
      begin
        match Amailbox.selected_mbox context.!mailbox with
        | Some mailbox -> Some (user,mailbox)
        | None -> Some (user,"")
      end
  | None -> None

let handle_idle context =
  response context None (Resp_Any ("+ idling")) None

let unset_recent context =
  match Amailbox.selected_mbox context.!mailbox with
  | Some mailbox -> 
    Amailbox.store context.!mailbox (fun () -> return ()) (fun _ -> return ())
    (Interpreter.get_sequence "1:*") Store_MinusFlagsSilent [Flags_Recent] None false >>= fun _ -> return ()
  | None -> return ()

let get_client_ids l =
  let rec get l acc =
    match l with
    | n :: v :: tl -> let acc = (n,v) :: acc in get tl acc
    | _ -> acc
  in
  get l []

let handle_id context l =
  context.client_id := get_client_ids l;
  write_resp context.id context.!netw (Resp_Untagged (formated_id(Configuration.id))) >>
  response context None (Resp_Ok (None, "ID completed")) None

let handle_capability context = 
  begin
  if (Amailbox.user context.!mailbox) = None then
    write_resp context.id context.!netw (Resp_Untagged (formated_capability(Configuration.capability)))
  else
    write_resp context.id context.!netw (Resp_Untagged (formated_capability(Configuration.auth_capability)))
  end >>
  response context None (Resp_Ok (None, "CAPABILITY completed")) None

let handle_logout context =
  unset_recent context >>
  write_resp context.id context.!netw (Resp_Bye(None,"")) >>
  response context (Some State_Logout) (Resp_Ok (None, "LOGOUT completed")) None

let handle_enable capability context =
  begin
  if context.!state = State_Notauthenticated then
    write_resp context.id context.!netw (Resp_Untagged ("ENABLED")) >>
    return "ENABLE ignored in non-authenticated state."
  else (
    context.capability := capability :: context.!capability;
    return "ENABLED"
  )
  end >>= fun msg ->
  response context None (Resp_Ok (None, msg)) None

let unsolicited_response context (status:Storage_meta.mailbox_metadata) =
  (* also need to output changes "* EXPUNGE.." *)
  let resp_writer = (fun str -> (* remove modseq from the fetch response *)
    write_resp_untagged context.id context.!netw
    (replace ~regx:"MODSEQ ([0-9]+) " ~tmpl:"" str)) in 
  let resp_prefix = (fun () -> return ()) in
  resp_writer (Printf.sprintf "%d EXISTS" status.count) >>
  resp_writer (Printf.sprintf "%d RECENT" status.recent) >>
  Amailbox.fetch context.!mailbox resp_prefix resp_writer 
   ([SeqRange (Number 1,Wild)]) (FetchAtt [Fetch_Flags]) 
   (Some context.!noop_modseq) false >>= fun _ ->
  context.noop_modseq := status.modseq;
  return ()

(** TBD should have a hook into the maintenance to recet inactivity **)
let handle_noop context =
  (* check if modseq changed *)
  let open Storage_meta in
  begin
  examine context >>= function
  |`Ok(_,status) -> 
    (* has modseq for the same mailbox been changed by another client? *) 
    if Int64.compare context.!noop_modseq status.modseq <> 0 then (
      unsolicited_response context status >>= fun () ->
      (* noop resets highestmodseq *)
      context.highestmodseq := `Highestmodseq;
      return ()
    ) else
      return ()
  | _ -> return ()
  end >>
  response context None (Resp_Ok (None, "NOOP completed")) None

let handle_done context =
  response context None (Resp_Ok (None, "IDLE")) None

(**
 * Not Authenticated state
**)
let handle_authenticate context auth_type text =
  begin
  match text with 
  | Some text -> return text
  | None ->
    write_resp context.id context.!netw (Resp_Cont("")) >>
    Lwt.pick [
      Lwt_mutex.lock context.client_timed_out >> raise ClientTimedOut;
      Lwt_io.read_line context.!netr
    ] >>= fun text ->
    Log_.log `Info3 (Printf.sprintf "----> %s: %s\n" (Int64.to_string context.id) text);
    return text
  end >>= fun text ->
  Account.authenticate auth_type text >>= function
    | `Ok (m,u,p) -> response context (Some State_Authenticated) m (Some
    (Amailbox.create context.config u (Some p)))
    | `Error e -> response context None e None

let handle_login context user password =
  Account.login user password >>= function
    | `Ok (m,u,p) -> response context (Some State_Authenticated) m (Some
    (Amailbox.create context.config u (Some p)))
    | `Error e -> response context None e None

let handle_starttls context =
 let open Server_config in
 if context.config.starttls = true then (
   context.Context.starttls () >>= fun (r,w) ->
   context.netr := r;
   context.netw := w;
   response context None (Resp_Ok(None,"STARTTLS")) None
 ) else
   response context None (Resp_Bad(None,"")) None
(**
 * Done Not Authenticated state
**)

(**
 * Authenticated state
**)

let quote_file file =
  if match_regex file ~regx:"[ ]" then
    "\"" ^ file ^ "\""
  else
    file

let list_resp flags file =
  let flags_str = String.concat " " flags in
  let l = List.concat [["LIST ("]; [flags_str]; [") \"/\" "]; [quote_file file]] in 
  Resp_Untagged(String.concat "" l)

let handle_list context reference mailbox lsub =
  begin
  if lsub = false then
    Amailbox.list context.!mailbox reference mailbox
  else
    Amailbox.lsub context.!mailbox reference mailbox
  end >>= fun l ->
  Lwt_list.iter_s (fun (file, flags) ->
      write_resp context.id context.!netw (list_resp flags file)
  ) l >>
  response context None (Resp_Ok(None, "LIST completed")) None

(** review - where the flags are coming from TBD **)
let handle_select context mailbox condstore rw =
  (if rw then
    Amailbox.select context.!mailbox mailbox
  else
    Amailbox.examine context.!mailbox mailbox
  ) >>= function
  | `NotExists -> response context None (Resp_No(None,"Mailbox doesn't exist:" ^ mailbox)) None
  | `NotSelectable ->  response context None (Resp_No(None,"Mailbox is not selectable :" ^ mailbox)) None
  | `Ok (mbx, header) ->
    let open Storage_meta in
    if header.uidvalidity = "" then (** give up TBD **)
      response context None (Resp_No(None,"Uidvalidity failed")) None
    else
    (
      (* might be tricky, it is probably shared by all clients which select this
       * mailbox
       *)
      unset_recent context >>= fun () ->
      context.highestmodseq := `Sessionstart;
      context.noop_modseq := header.modseq;
      let (flags,prmnt_flags) = Configuration.get_mbox_flags in
      let flags = to_plist (String.concat " " flags) in
      let pflags = to_plist (String.concat " " prmnt_flags) in
      write_resp context.id context.!netw (Resp_Untagged ("FLAGS " ^ flags)) >>
      write_resp context.id context.!netw (Resp_Ok (Some RespCode_Permanentflags, pflags)) >>
      write_resp context.id context.!netw (Resp_Untagged ((string_of_int header.count) ^ " EXISTS")) >>
      write_resp context.id context.!netw (Resp_Untagged ((string_of_int header.recent) ^ " RECENT")) >>
      write_resp context.id context.!netw (Resp_Ok (Some RespCode_Uidvalidity, header.uidvalidity)) >>
      write_resp context.id context.!netw (Resp_Ok (Some RespCode_Uidnext, string_of_int header.uidnext)) >>
      write_resp context.id context.!netw (Resp_Ok (Some RespCode_Highestmodseq, Int64.to_string header.modseq)) >>
      begin
      if header.unseen <> 0 then
        write_resp context.id context.!netw (Resp_Ok (Some RespCode_Unseen, string_of_int header.unseen))
      else
        return ()
      end >>
      begin 
      if rw then
        response context (Some State_Selected) (Resp_Ok(Some RespCode_Read_write, "")) (Some mbx)
      else
        response context (Some State_Selected) (Resp_Ok(Some RespCode_Read_only, "")) (Some mbx)
      end
    )

(** create a mailbox **)
let handle_create context mailbox =
  Amailbox.create_mailbox context.!mailbox mailbox >>= function
    | `Ok -> response context None (Resp_Ok(None, "CREATE completed")) None
    | `Error e -> response context None (Resp_No(None,e)) None

(** delete a mailbox **)
let handle_delete context mailbox =
  Amailbox.delete_mailbox context.!mailbox mailbox >>= function
    | `Ok -> response context None (Resp_Ok(None, "DELETE completed")) None
    | `Error e -> response context None (Resp_No(None,e)) None

(** rename a mailbox **)
let handle_rename context src dest = 
  Amailbox.rename_mailbox context.!mailbox src dest >>= function
    | `Ok -> response context None (Resp_Ok(None, "RENAME completed")) None
    | `Error e -> response context None (Resp_No(None,e)) None

(** subscribe a mailbox **)
let handle_subscribe context mailbox = 
  Amailbox.subscribe context.!mailbox mailbox >>= function
    | `Ok -> response context None (Resp_Ok(None, "SUBSCRIBE completed")) None
    | `Error e -> response context None (Resp_No(None,e)) None

(** subscribe a mailbox **)
let handle_unsubscribe context mailbox = 
  Amailbox.unsubscribe context.!mailbox mailbox >>= function
    | `Ok -> response context None (Resp_Ok(None, "UNSUBSCRIBE completed")) None
    | `Error e -> response context None (Resp_No(None,e)) None

let handle_status context mailbox optlist =
  let open Storage_meta in
  Amailbox.examine context.!mailbox mailbox >>= function
  | `NotExists -> response context None (Resp_No(None,"Mailbox doesn't exist:" ^ mailbox)) None
  | `NotSelectable ->  response context None (Resp_No(None,"Mailbox is not selectable :" ^ mailbox)) None
  | `Ok (mbx, header) ->
  if header.uidvalidity = "" then (** give up TBD **)
  (
    response context None (Resp_No(None,"Uidvalidity failed")) None
  )
  else
  (
    let output = (List.fold_left (fun acc opt ->
      let str = (match opt with
      | Stat_Highestmodseq -> "HIGHESTMODSEQ " ^ (Int64.to_string header.modseq)
      | Stat_Messages -> "EXISTS " ^ (string_of_int header.count)
      | Stat_Recent -> "RECENT " ^ (string_of_int header.recent)
      | Stat_Uidnext -> "UIDNEXT " ^(string_of_int header.uidnext)
      | Stat_Uidvalidity -> "UIDVALIDITY " ^ header.uidvalidity
      | Stat_Unseen -> "UNSEEN " ^ (string_of_int header.nunseen)
      ) in 
      if acc = "" then
        acc ^ str
      else
        acc ^ " " ^ str
    ) "" optlist) in
    let prefix = ("STATUS " ^ (quote ~always:false mailbox) ^ " ") in
    let resp = prefix ^ (to_plist output) in
    write_resp context.id context.!netw (Resp_Untagged resp) >>
    response context None (Resp_Ok(None, "STATUS completed")) None
  )

(* send unsolicited response to idle clients *)
let idle_clients mailbox context =
  let open Storage_meta in
  match get_selected context with
  | Some (user,_) ->
    begin
    Amailbox.examine context.!mailbox mailbox >>= function
    |`Ok(_,status) -> 
      Connections.fold (fun acc (ctx:context) ->
        acc >>= fun () ->
        match get_selected ctx with
        | Some (ctx_user, ctx_mailbox) (* if not self and another client for same user/selected mbox *)
            when context.id <> ctx.id && ctx_user = user && mailbox = ctx_mailbox ->
          if Stack.is_empty ctx.!commands = false && is_idle (Stack.top ctx.!commands) then ( (* idle command is in progress for another client *)
            unsolicited_response ctx status >>
            write_resp_untagged context.id ctx.!netw ("Ok still here")
          ) else
            return ()
        | _ -> return ()
      ) (return())
    |_ -> return ()
    end
  |None -> return ()

(** handle append **)
let handle_append context mailbox flags date literal =
  (** is the size sane? **)
  let size = (match literal with
  | Literal n -> n
  | LiteralPlus n -> n) in
  let open Server_config in
  if size > context.config.max_msg_size then
    response context None (Resp_No(None,"Max message size")) None
  else (
    Amailbox.append context.!mailbox mailbox context.!netr context.!netw flags date literal >>= function
      | `NotExists -> response context None (Resp_No(Some RespCode_Trycreate,"")) None
      | `NotSelectable -> response context None (Resp_No(Some RespCode_Trycreate,"Noselect")) None
      | `Error e -> response context None (Resp_No(None,e)) None
      | `Eof i -> response context (Some State_Logout) (Resp_No(None, "Truncated Message")) None
      | `Ok -> 
        idle_clients mailbox context >>= fun () ->
        response context None (Resp_Ok(None, "APPEND completed")) None
  )

(**
 * Done Authenticated state
**)

(**
 * Selected state
**)

let handle_close context =
  unset_recent context >>= fun () ->
  context.highestmodseq := `None;
  let mbx = Amailbox.close context.!mailbox in
  response context (Some State_Authenticated) (Resp_Ok(None, "CLOSE completed")) (Some mbx)

let rec print_search_tree t indent =
  let indent = indent ^ " " in
  let open Amailbox in
  match t with
  | Key k -> Printf.printf "%s-key\n%!" indent
  | KeyList k -> Printf.printf "%s-key list %d\n%!" indent (List.length k);
    List.iter (fun i -> print_search_tree i indent) k
  | NotKey k -> Printf.printf "%s-key not\n%!" indent; print_search_tree k indent
  | OrKey (k1,k2) -> Printf.printf "%s-key or\n%!" indent; print_search_tree k1 indent; print_search_tree k2 indent

(** handle the charset TBD **)
let handle_search context charset search buid =
  let resp_prefix = return in
  let t = Unix.gettimeofday () in
  Amailbox.search context.!mailbox resp_prefix search buid >>= function 
    (** what do these two states mean in this contex? TBD **)
  | `NotExists -> response context None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  response context None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> response context None (Resp_No(None,e)) None
  | `Ok (modseq,r) -> 
    let modseq =
      match modseq with
      |None -> ""
      |Some modseq -> " (MODSEQ " ^ (Int64.to_string modseq) ^ ")"
    in
    let prefix = "SEARCH " in
    write_resp context.id context.!netw (Resp_Untagged (prefix ^ (List.fold_left (fun acc i ->
      let s = string_of_int i in
      if acc = "" then 
        s 
      else 
        s ^ " " ^ acc) "" r
    ) ^ modseq)) >>
    let resp = Printf.sprintf "SEARCH completed %02fsec" (Unix.gettimeofday () -. t) in
    response context None (Resp_Ok(None, resp)) None

let handle_fetch context sequence fetchattr changedsince buid =
  let resp_prefix = resp_highestmodseq (changedsince <> None) context in
  let t = Unix.gettimeofday () in
  Amailbox.fetch context.!mailbox resp_prefix (write_resp_untagged
      context.id context.!netw) sequence fetchattr changedsince buid >>= function
  | `NotExists -> response context None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  response context None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> response context None (Resp_No(None,e)) None
  | `Ok ->
    let resp = Printf.sprintf "FETCH completed %02fsec" (Unix.gettimeofday () -. t) in
    response context None (Resp_Ok(None, resp)) None

let handle_store context sequence flagsatt flagsval changedsince buid =
  let resp_prefix = resp_highestmodseq (changedsince <> None) context in
  Amailbox.store context.!mailbox resp_prefix (write_resp_untagged context.id context.!netw) sequence
      flagsatt flagsval changedsince buid >>= function
  | `NotExists -> response context None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  response context None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> response context None (Resp_No(None,e)) None
  | `Ok modified ->
    let conditional = if changedsince = None then "" else "conditional " in
    let (success,modified) = 
      if List.length modified = 0 then
        "completed",""
      else
        "failed","[MODIFIED " ^ (String.concat "," modified) ^ "] " in
    idle_clients (selected_mailbox_exn context.!mailbox) context >>= fun () ->
    response context None (Resp_Ok(None, modified ^ conditional ^ "STORE " ^ success)) None

let handle_copy context sequence mailbox buid =
  Amailbox.copy context.!mailbox mailbox sequence buid >>= function
  | `NotExists -> response context None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  response context None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> response context None (Resp_No(None,e)) None
  | `Ok -> 
    idle_clients mailbox context >>
    response context None (Resp_Ok(None, "COPY completed")) None

let handle_expunge context =
  Amailbox.expunge context.!mailbox (write_resp_untagged context.id context.!netw) >>= function
  | `NotExists -> response context  None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  response context  None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> response context None (Resp_No(None,e)) None
  | `Ok -> 
    idle_clients (selected_mailbox_exn context.!mailbox) context >>
    response context None (Resp_Ok(None, "EXPUNGE completed")) None

(**
 * Done Selected state
**)

let handle_any context = function
  | Cmd_Id l -> handle_id context l
  | Cmd_Capability -> handle_capability context
  | Cmd_Noop -> handle_noop context
  | Cmd_Logout -> handle_logout  context
  | Cmd_Enable capability -> handle_enable capability context

let handle_notauthenticated context = function
  | Cmd_Authenticate (a,s) -> handle_authenticate context a s 
  | Cmd_Login (u, p) -> handle_login context u p 
  | Cmd_Starttls -> handle_starttls context
  | Cmd_Lappend (user,pswd,mailbox,literal) -> 
      let mbx = Amailbox.create context.config user pswd in
      let context = {context with mailbox = ref mbx} in
      handle_append context mailbox None None literal 

let handle_authenticated context = function
  | Cmd_Select (mailbox,condstore) -> handle_select context mailbox condstore true
  | Cmd_Examine (mailbox,condstore) -> handle_select context mailbox condstore false
  | Cmd_Create mailbox -> handle_create context mailbox 
  | Cmd_Delete mailbox -> handle_delete context mailbox 
  | Cmd_Rename (mailbox,to_mailbox) -> handle_rename context mailbox to_mailbox 
  | Cmd_Subscribe mailbox -> handle_subscribe context mailbox
  | Cmd_Unsubscribe mailbox -> handle_unsubscribe context mailbox 
  | Cmd_List (reference, mailbox) -> handle_list context reference mailbox false
  | Cmd_Lsub (reference, mailbox) -> handle_list context reference mailbox true
  | Cmd_Status (mailbox,optlist) -> handle_status context mailbox optlist 
  | Cmd_Append (mailbox,flags,date,size) -> handle_append context mailbox flags date size 
  | Cmd_Idle -> handle_idle context
  | Cmd_Done -> handle_done context

let handle_selected context = function
  | Cmd_Check -> response context None (Resp_Ok(None, "CHECK completed")) None
  | Cmd_Close -> handle_close context
  | Cmd_Expunge -> handle_expunge context
  | Cmd_Search (charset,search, buid) -> handle_search context charset search buid
  | Cmd_Fetch (sequence,fetchattr, changedsince, buid) -> handle_fetch context sequence fetchattr changedsince buid 
  | Cmd_Store (sequence,flagsatt,flagsval, unchanged, buid) -> 
      handle_store context sequence flagsatt flagsval unchanged buid 
  | Cmd_Copy (sequence,mailbox, buid) -> handle_copy context sequence mailbox buid 

let handle_command context =
  let state = context.!state in
  let command = (Stack.top context.!commands).command in
  match command with
  | Any r -> handle_any context r
  | Notauthenticated r when state = State_Notauthenticated-> 
    handle_notauthenticated context r
  | Authenticated r when state = State_Authenticated || state = State_Selected -> 
    handle_authenticated context r
  | Selected r when state = State_Selected -> 
    handle_selected context r
  | Done -> response context (Some State_Logout) (Resp_Bad(None,"")) None
  | _ -> response context None (Resp_Bad(None, "Bad Command")) None

(* read a line from the network
 * if the line ends with literal {N} and it is not the append
 * then read N bytes, otherwise return the buffer
 *)
let rec read_network context buffer =
  begin
  catch ( fun () ->
    Lwt.pick [
      Lwt_mutex.lock context.client_timed_out >> return `Done;
      Lwt_io.read_line_opt context.!netr >>= function
      | None -> return `None
      | Some buff -> return (`Ok buff)
    ]
  )
  (fun ex -> match ex with
    | End_of_file -> 
      Log_.log `Info1 (Printf.sprintf "### received EOF on network read %s\n" (Int64.to_string context.id)); 
      return `Done
    | _ -> raise ex
  )
  end >>= function
  | `Done -> return `Done
  | `None -> 
    if Buffer.length buffer > 0 then (
      context.client_last_active := Unix.gettimeofday ();
      return (`Ok (Buffer.contents buffer))
    ) else
      return `Done
  | `Ok buff ->
  context.client_last_active := Unix.gettimeofday ();
  (** does command end in the literal {[0-9]+} ? **)
  let i = match_regex_i buff ~regx:"{\\([0-9]+\\)[+]?}$" in
  if i < 0 then (
    Buffer.add_string buffer buff;
    Buffer.add_string buffer "\r\n";
    return (`Ok (Buffer.contents buffer))
  ) else (
    (** literal's size **)
    let len = int_of_string (Str.matched_group 1 buff) in
    (** buffer's content up to the literal **)
    let sub = Str.string_before buff i in
    let literal = Str.string_after buff i in
    Buffer.add_string buffer sub;
    if match_regex ~case:false (Buffer.contents buffer) ~regx:append_regex ||
      match_regex ~case:false (Buffer.contents buffer) ~regx:lappend_regex then (
      Buffer.add_string buffer literal;
      Buffer.add_string buffer "\r\n";
      return (`Ok (Buffer.contents buffer))
    ) else if ((Buffer.length buffer) + len) > 10240 then (
      return (`Error ((String.sub (Buffer.contents buffer) 0 500) ^ " command too long"))
    ) else (
      (if match_regex literal ~regx:"[+]}$" = false then
        write_resp context.id context.!netw (Resp_Cont(""))
      else
        return ()
      ) >>
      let str = String.create len in
      Lwt.pick [
        Lwt_mutex.lock context.client_timed_out >> return `Done;
        Lwt_unix.sleep 5.0 >> return `Timeout; 
        Lwt_io.read_into_exactly context.!netr str 0 len >> return (`Ok str)
      ] >>= function
      | `Ok str ->
        Buffer.add_string buffer str;
        read_network context buffer
      | `Timeout ->
        return (`Error "timeout")
      | `Done -> return `Done
    )
  )

let dolog buff cmd context =
  let log_msg =
  match cmd.command with
  | Notauthenticated cmd ->
    begin
    match cmd with
    | Cmd_Authenticate _ -> "AUTHENTICATE ..."
    | Cmd_Login _ -> "LOGIN ..."
    | Cmd_Lappend _ -> "A LAPPEND ..."
    | _ -> buff
    end
  | _ -> buff
  in
  Log_.log `Info3 (Printf.sprintf "----> %s: %s\n" (Int64.to_string context.id) log_msg)

let get_command msgt context =
  let open Parsing in
  let open Lexing in
  let open Lex in
  catch (fun () ->
    let buffer = Buffer.create 0 in
    read_network context buffer >>= function
    | `Done -> return `Done
    | `Error err -> return (`Error err)
    | `Ok buff ->
    let lexbuff = Lexing.from_string buff in
    let current_cmd = 
    (
      let current_cmd = 
        try
          (Parser.request (Lex.read (ref `Tag)) lexbuff)
        with Parser.Error -> 
          raise (SyntaxError ("bad command, parser: " ^ (try String.sub buff 0 100 with _-> buff)))
      in
      dolog buff current_cmd context;
      (* if last command idle then next could only be done *)
      if Stack.is_empty context.!commands then
        current_cmd
      else (
        let last_cmd = Stack.top context.!commands in
        if is_idle last_cmd then (
          if is_done current_cmd = false then
            raise ExpectedDone 
          else (*tag from idle goes into done *)
            {current_cmd with tag = last_cmd.tag}
        ) else
          current_cmd
      )
    ) in
    (try
      let _ = Stack.pop context.!commands in ()
    with _ -> ());
    Stack.push current_cmd context.!commands ;
    return (`Ok )
  )
  (function 
  | SyntaxError e -> return (`Error (e))
  | Parser.Error -> return (`Error ("bad command, parser"))
  | Interpreter.InvalidSequence -> return (`Error ("bad command, invalid sequence"))
  | Dates.InvalidDate -> return (`Error("bad command, invalid date"))
  | ExpectedDone -> return (`Error("Expected DONE"))
  | e -> return (`Error(Printexc.get_backtrace()))
  )

let rec client_requests msgt context =
  catch ( fun () ->
    get_command msgt context >>= function
    | `Done -> return `Done
    | `Error e -> 
      Log_.log `Error (e ^ "\n");
      write_resp context.id context.!netw (Resp_Bad(None,e)) >> client_requests msgt context
    | `Ok -> handle_command context >>= fun response ->
      if context.!state = State_Logout then
        return `Done
      else (
        let command = Stack.top context.!commands in
        write_resp context.id context.!netw ~tag:command.tag response >> client_requests msgt context
      )
  )
  (fun ex -> Log_.log `Error (Printf.sprintf "client_requests exception: %s\n"
    (Printexc.to_string ex)); return `Done)

(* scheduled maintenance 
 * - send ping to idle clients
 *)
let rec maintenance config =
  let open Server_config in
  catch (fun () ->
    Lwt_unix.sleep config.idle_interval >>
    Connections.fold (fun acc ctx ->
      acc >>= fun () ->
      let now = Unix.gettimeofday () in
      if (now -. ctx.!client_last_active > 1800.) then (
        Log_.log `Info1 (Printf.sprintf "### client %s timed-out\n" (Int64.to_string ctx.id));
        write_resp ctx.id ctx.!netw (Resp_Bye(None,"Autologout; idle for too long")) >>= fun () ->
        Lwt_mutex.unlock ctx.client_timed_out;
        return ()
      ) else if (Stack.is_empty ctx.!commands = false) && is_idle (Stack.top ctx.!commands) then (
        write_resp_untagged ctx.id ctx.!netw "OK still here"
      ) else
        return ()
    ) (return())
  ) 
  (fun ex -> 
    Log_.log `Error (Printf.sprintf "### maintenance error: %s\n" (Printexc.to_string ex));
    return ()
  ) >> maintenance config
