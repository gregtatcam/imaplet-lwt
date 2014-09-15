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
open Core.Std
open BatLog
open Imaplet_types
open Response
open Regex

exception SystemError of string
exception ExpectedDone

(*
let make_resp_ctx resp_state_ctx resp_ctx resp_mbx_ctx =
  {resp_state_ctx; resp_ctx; resp_mbx_ctx};;

let return_resp_ctx resp_state_ctx resp_ctx resp_mbx_ctx =
  return (make_resp_ctx resp_state_ctx resp_ctx resp_mbx_ctx);;

(** parse the buffer, return ok (request) or error (msg) **)
let get_request_context contexts buff =
  printf "get_request_context -%s-\n%!" buff; Out_channel.flush stdout;
  try
    let lexbuff = Lexing.from_string buff in
    let command = (Parser.request (Lex.read (ref `Tag)) lexbuff) in
    printf "get_request_context, returned from parser\n%!"; Out_channel.flush stdout;
    let idle = find_idle contexts.req_ctx in
    let isdone = is_done command in
    if idle <> None && isdone = false then
      raise ExpectedDone
    else (
      let t =
      (
        if isdone then
          (match idle with Some idle -> idle.request.tag | None -> "")
        else
          command.tag
      ) in
      Ok ({request={command with tag = t}})
    )
  with
  | SyntaxError e -> printf "get_request_context error %s\n%!" e; Error (e)
  | Parser.Error -> printf "get_request_context bad command\n%!"; Error ("bad command, parser")
  | Interpreter.InvalidSequence -> Error ("bad command, invalid sequence")
  | Regex.InvalidDate -> Error("bad command, invalid date")
  | ExpectedDone -> Error("Expected DONE")
  | e -> Error(Exn.backtrace())

(** handle all commands
 * return either Ok, Bad, No, Preauth, Bye, or Continue response
**)
(**
 * Any state
**)
let handle_id writer l =
  write_resp writer (Resp_Untagged (formated_id(Configuration.id)));
  return_resp_ctx None (Resp_Ok (None, "ID completed")) None

let handle_capability mbx writer = 
  (if (Amailbox.user mbx) = None then
    write_resp writer (Resp_Untagged (formated_capability(Configuration.capability)))
  else
    write_resp writer (Resp_Untagged (formated_capability(Configuration.auth_capability))));
  return_resp_ctx None (Resp_Ok (None, "CAPABILITY completed")) None

let handle_logout ipc_ctx =
  write_resp ipc_ctx.net_w (Resp_Bye(None,""));
  return_resp_ctx (Some State_Logout) (Resp_Ok (None, "LOGOUT completed")) None

(** TBD should have a hook into the maintenance to recet inactivity **)
let handle_noop () =
  return_resp_ctx None (Resp_Ok (None, "NOOP completed")) None

(** TBD, needs work, server should send updates while idle is active (terminated
 * by clien'ts done)
**)
let handle_idle contexts ipc_ctx =
  (match Amailbox.user contexts.mbx_ctx with
  | Some user -> 
    Printf.printf "handle_idle ======== %s %s\n%!" (Int64.to_string ipc_ctx.connid) user;
    ipc_ctx.connections := (ipc_ctx.connid,user,ipc_ctx.net_w) :: ipc_ctx.!connections
  | None -> ());
  return_resp_ctx None (Resp_Any ("+ idling")) None

let handle_done contexts ipc_ctx =
  ipc_ctx.connections := List.fold ipc_ctx.!connections ~init:[] ~f:(fun acc i ->
    let (cid,u,_) = i in
    if Int64.equal ipc_ctx.connid cid then (
      Printf.printf "handle_done removing ======== %s %s\n%!" (Int64.to_string cid) u;
      acc
    ) else
      i :: acc
  );
  return_resp_ctx None (Resp_Ok (None, "IDLE")) None

(**
 * Not Authenticated state
**)
let handle_authenticate auth_type text ipc_ctx =
  Account.authenticate ipc_ctx.net_w auth_type text >>= function
    | Ok (m,u) -> return_resp_ctx (Some State_Authenticated) m (Some (Amailbox.create u ipc_ctx.str_rw))
    | Error e -> return_resp_ctx None e None

let handle_login user password ipc_ctx =
  Account.login ipc_ctx.net_w user password >>= function
    | Ok (m,u) -> return_resp_ctx (Some State_Authenticated) m (Some (Amailbox.create u ipc_ctx.str_rw))
    | Error e -> return_resp_ctx None e None
(**
 * Done Not Authenticated state
**)

(**
 * Authenticated state
**)

let quote_file file =
  if match_regex file "[ ]" then
    "\"" ^ file ^ "\""
  else
    file

let list_resp flags file =
  let flags_str = String.concat ~sep:" " flags in
  let l = List.concat [["LIST ("]; [flags_str]; [") \"/\" "]; [quote_file file]] in 
  Resp_Untagged(String.concat ~sep:"" l)

let handle_list writer reference mailbox contexts lsub =
  (if lsub = false then
    Amailbox.listmbx contexts.mbx_ctx reference mailbox
  else
    Amailbox.lsubmbx contexts.mbx_ctx reference mailbox
  )>>= fun l ->
    List.iter l 
    ~f:(fun (file, flags) ->
      write_resp writer (list_resp flags file)
    );
  return_resp_ctx None (Resp_Ok(None, "LIST completed")) None

(** review - where the flags are coming from TBD **)
let handle_select writer mailbox contexts rw =
  let open Amailbox in
  (if rw then
    select contexts.mbx_ctx mailbox
  else
    examine contexts.mbx_ctx mailbox
  ) >>= function
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist:" ^ mailbox)) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable :" ^ mailbox)) None
  | `Error e -> return_resp_ctx None (Resp_No(None, e)) None
  | `Ok (mbx, header) ->
    if header.uidvalidity = "" then (** give up TBD **)
    (
      return_resp_ctx None (Resp_No(None,"Uidvalidity failed")) None
    )
    else
    (
      let (flags,prmnt_flags) = Configuration.get_mbox_flags in
      let flags = to_plist (String.concat ~sep:" " flags) in
      write_resp writer (Resp_Untagged ("FLAGS " ^ flags));
      let flags = to_plist (String.concat ~sep:" " prmnt_flags) in
      write_resp writer (Resp_Ok (Some RespCode_Permanentflags, flags));
      write_resp writer (Resp_Untagged ((string_of_int header.count) ^ " EXISTS"));
      write_resp writer (Resp_Untagged ((string_of_int header.recent) ^ " RECENT"));
      write_resp writer (Resp_Ok (Some RespCode_Uidvalidity, header.uidvalidity));
      write_resp writer (Resp_Ok (Some RespCode_Uidnext, string_of_int header.uidnext));
      if rw then
        return_resp_ctx (Some State_Selected) (Resp_Ok(Some RespCode_Read_write, "")) (Some mbx)
      else
        return_resp_ctx (Some State_Selected) (Resp_Ok(Some RespCode_Read_only, "")) (Some mbx)
    )

(** create a mailbox **)
let handle_create writer mailbox contexts =
  Amailbox.create_mailbox contexts.mbx_ctx mailbox >>= function
    | `Ok -> return_resp_ctx None (Resp_Ok(None, "CREATE completed")) None
    | `Error e -> return_resp_ctx None (Resp_No(None,e)) None

(** delete a mailbox **)
let handle_delete writer mailbox contexts =
  Amailbox.delete_mailbox contexts.mbx_ctx mailbox >>= function
    | `Ok -> return_resp_ctx None (Resp_Ok(None, "DELETE completed")) None
    | `Error e -> return_resp_ctx None (Resp_No(None,e)) None

(** rename a mailbox **)
let handle_rename writer src dest contexts = 
  Amailbox.rename_mailbox contexts.mbx_ctx src dest >>= function
    | `Ok -> return_resp_ctx None (Resp_Ok(None, "RENAME completed")) None
    | `Error e -> return_resp_ctx None (Resp_No(None,e)) None

(** subscribe a mailbox **)
let handle_subscribe writer mailbox contexts = 
  Amailbox.subscribe contexts.mbx_ctx mailbox >>= function
    | `Ok -> return_resp_ctx None (Resp_Ok(None, "SUBSCRIBE completed")) None
    | `Error e -> return_resp_ctx None (Resp_No(None,e)) None

(** subscribe a mailbox **)
let handle_unsubscribe writer mailbox contexts = 
  Amailbox.unsubscribe contexts.mbx_ctx mailbox >>= function
    | `Ok -> return_resp_ctx None (Resp_Ok(None, "UNSUBSCRIBE completed")) None
    | `Error e -> return_resp_ctx None (Resp_No(None,e)) None

let handle_status writer mailbox optlist contexts =
  let open Amailbox in
    examine contexts.mbx_ctx mailbox >>= function
    | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist:" ^ mailbox)) None
    | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable :" ^ mailbox)) None
    | `Error e -> return_resp_ctx None (Resp_No(None, e)) None
    | `Ok (mbx, header) ->
    if header.uidvalidity = "" then (** give up TBD **)
    (
      return_resp_ctx None (Resp_No(None,"Uidvalidity failed")) None
    )
    else
    (
      let output = (List.fold optlist ~init:"" ~f:(fun acc opt ->
        let str = (match opt with
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
      )) in
      write_resp writer (Resp_Untagged (to_plist output));
      return_resp_ctx None (Resp_Ok(None, "STATUS completed")) None
    )

(* send unsolicited response to idle clients *)
let idle_clients contexts ipc_ctx =
  let open Amailbox in
  let get_status () =
   match selected_mbox contexts.mbx_ctx with
   | Some mailbox ->
    (examine contexts.mbx_ctx mailbox >>= function
    |`Ok(mbx,header) -> return (Some header)
    | _ -> return None
    )
   | None -> return None
  in
  match user contexts.mbx_ctx with
  |Some user ->
    let idle = List.fold ipc_ctx.!connections ~init:[] ~f:(fun acc i ->
      let (_,u,_) = i in
      if u = user then 
        i :: acc
      else
        acc
    ) in
    if List.length idle > 0 then (
      get_status () >>= function
      | Some status ->
        List.iter idle ~f:(fun i ->
         let (id,u,w) = i in
         if u = user then (
           Printf.printf "=========== idle_clients %s %s\n%!" (Int64.to_string id) u;
           write_resp_untagged w ("EXISTS " ^ (string_of_int status.count));
           write_resp_untagged w ("RECENT " ^ (string_of_int status.recent))
         ) else (
           ()
         )
        ); return ()
      | None -> return ()
    ) else (
      return ()
    )
  |None -> return ()

(** handle append **)
let handle_append ipc_ctx mailbox flags date literal contexts =
  printf "handle_append\n%!";
  let open Amailbox in
  (** is the size sane? **)
  let size = (match literal with
  | Literal n -> n
  | LiteralPlus n -> n) in
  let open ServerConfig in
  if size > srv_config.max_msg_size then
    return_resp_ctx None (Resp_No(None,"Max message size")) None
  else (
    append contexts.mbx_ctx mailbox ipc_ctx.net_r ipc_ctx.net_w flags date literal >>= function
      | `NotExists -> return_resp_ctx None (Resp_No(Some RespCode_Trycreate,"")) None
      | `NotSelectable -> return_resp_ctx None (Resp_No(Some RespCode_Trycreate,"Noselect")) None
      | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
      | `Eof i -> return_resp_ctx (Some State_Logout) (Resp_No(None, "Truncated Message")) None
      | `Ok -> 
        idle_clients contexts ipc_ctx >>= fun () ->
        return_resp_ctx None (Resp_Ok(None, "APPEND completed")) None
  )

(**
 * Done Authenticated state
**)

(**
 * Selected state
**)

let handle_close writer contexts context =
  let mbx = Amailbox.close contexts.mbx_ctx in
  return_resp_ctx (Some State_Authenticated) (Resp_Ok(None, "CLOSE completed")) (Some mbx)

let rec print_search_tree t indent =
  printf "search ------\n%!";
  let indent = indent ^ " " in
  let open Amailbox in
  match t with
  | Key k -> printf "%s-key\n%!" indent
  | KeyList k -> printf "%s-key list %d\n%!" indent (List.length k);List.iter k ~f:(fun i -> print_search_tree i indent)
  | NotKey k -> printf "%s-key not\n%!" indent; print_search_tree k indent
  | OrKey (k1,k2) -> printf "%s-key or\n%!" indent; print_search_tree k1 indent; print_search_tree k2 indent

(** handle the charset TBD **)
let handle_search writer charset search buid context =
  Amailbox.search context.mbx_ctx search buid >>= function 
    (** what do these two states mean in this contex? TBD **)
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
  | `Ok r -> 
      write_resp writer (Resp_Untagged (List.fold r ~init:""  ~f:(fun acc i ->
        let s = string_of_int i in
        if acc = "" then 
          s 
        else 
          s ^ " " ^ acc)
      ));
      return_resp_ctx None (Resp_Ok(None, "SEARCH completed")) None

let handle_fetch writer sequence fetchattr buid context =
  printf "handle_fetch\n";
  Amailbox.fetch context.mbx_ctx (write_resp_untagged writer) sequence fetchattr buid >>= function
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
  | `Ok -> return_resp_ctx None (Resp_Ok(None, "FETCH completed")) None

let handle_store ipc_ctx sequence flagsatt flagsval buid contexts =
  printf "handle_store %d %d\n" (List.length sequence) (List.length flagsval);
  Amailbox.store contexts.mbx_ctx (write_resp_untagged ipc_ctx.net_w) sequence flagsatt flagsval buid >>= function
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
  | `Ok ->
    idle_clients contexts ipc_ctx >>= fun () ->
    return_resp_ctx None (Resp_Ok(None, "STORE completed")) None

let handle_copy writer sequence mailbox buid contexts =
  printf "handle_copy %d %s\n" (List.length sequence) mailbox;
  Amailbox.copy contexts.mbx_ctx mailbox sequence buid >>= function
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
  | `Ok -> return_resp_ctx None (Resp_Ok(None, "COPY completed")) None

let handle_expunge writer contexts =
  printf "handle_expunge\n";
  Amailbox.expunge contexts.mbx_ctx (write_resp_untagged writer) >>= function
  (**
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable")) None
  **)
  | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
  | `Ok -> return_resp_ctx None (Resp_Ok(None, "EXPUNGE completed")) None

(**
 * Done Selected state
**)

let handle_any request contexts ipc_ctx context = match request with
  | Cmd_Id l -> handle_id ipc_ctx.net_w l
  | Cmd_Capability -> handle_capability contexts.mbx_ctx ipc_ctx.net_w
  | Cmd_Noop -> handle_noop()
  | Cmd_Logout -> handle_logout ipc_ctx

let handle_notauthenticated request contexts ipc_ctx context = match request with
  | Cmd_Authenticate (a,s) -> handle_authenticate a s ipc_ctx
  | Cmd_Login (u, p) -> handle_login u p ipc_ctx
  | Cmd_Starttls -> 
    let open ServerConfig in
    if srv_config.starttls = true then
      return_resp_ctx None (Resp_Ok(None,"STARTTLS")) None
    else
      return_resp_ctx None (Resp_Bad(None,"")) None
  | Cmd_Lappend (user,mailbox,literal) -> 
      let mbx = Amailbox.create user ipc_ctx.str_rw in
      let contexts = { contexts with mbx_ctx = mbx } in
      handle_append ipc_ctx mailbox None None literal contexts

let handle_authenticated request contexts ipc_ctx context = match request with
  | Cmd_Select mailbox -> handle_select ipc_ctx.net_w mailbox contexts true
  | Cmd_Examine mailbox -> handle_select ipc_ctx.net_w mailbox contexts false
  | Cmd_Create mailbox -> handle_create ipc_ctx.net_w mailbox contexts
  | Cmd_Delete mailbox -> handle_delete ipc_ctx.net_w mailbox contexts
  | Cmd_Rename (mailbox,to_mailbox) -> handle_rename ipc_ctx.net_w mailbox to_mailbox contexts
  | Cmd_Subscribe mailbox -> handle_subscribe ipc_ctx.net_w mailbox contexts
  | Cmd_Unsubscribe mailbox -> handle_unsubscribe ipc_ctx.net_w mailbox contexts
  | Cmd_List (reference, mailbox) -> handle_list ipc_ctx.net_w reference mailbox contexts false
  | Cmd_Lsub (reference, mailbox) -> handle_list ipc_ctx.net_w reference mailbox contexts true
  | Cmd_Status (mailbox,optlist) -> handle_status ipc_ctx.net_w mailbox optlist contexts
  | Cmd_Append (mailbox,flags,date,size) -> handle_append ipc_ctx mailbox flags date size contexts
  | Cmd_Idle -> handle_idle contexts ipc_ctx
  | Cmd_Done -> handle_done contexts ipc_ctx

let handle_selected request contexts ipc_ctx context = match request with
  | Cmd_Check -> return_resp_ctx None (Resp_Ok(None, "CHECK completed")) None
  | Cmd_Close -> handle_close ipc_ctx.net_w contexts context
  | Cmd_Expunge -> handle_expunge ipc_ctx.net_w contexts
  | Cmd_Search (charset,search, buid) -> handle_search ipc_ctx.net_w charset search buid contexts
  | Cmd_Fetch (sequence,fetchattr, buid) -> handle_fetch ipc_ctx.net_w sequence fetchattr buid contexts
  | Cmd_Store (sequence,flagsatt,flagsval, buid) -> 
      handle_store ipc_ctx sequence flagsatt flagsval buid contexts
  | Cmd_Copy (sequence,mailbox, buid) -> handle_copy ipc_ctx.net_w sequence mailbox buid contexts

let handle_commands contexts ipc_ctx context = 
  try_with ( fun () -> 
    let state = contexts.state_ctx in
    (
      match context.request.req with
      | Any r -> printf "handling any\n%!"; handle_any r contexts ipc_ctx context 
      | Notauthenticated r when state = State_Notauthenticated-> 
        printf "handling nonauthenticated\n%!"; handle_notauthenticated r contexts ipc_ctx context 
      | Authenticated r when state = State_Authenticated || state = State_Selected -> 
        printf "handling authenticated\n%!"; handle_authenticated r contexts ipc_ctx context 
      | Selected r when state = State_Selected -> 
        printf "handling selected\n%!"; handle_selected r contexts ipc_ctx context 
      | Done -> printf "Done, should log out\n%!"; 
        return_resp_ctx (Some State_Logout) (Resp_Bad(None,"")) None
      | _ -> return_resp_ctx None (Resp_Bad(None, "Bad Command")) None
    ) >>= fun rsp_ctx ->
    match rsp_ctx.resp_state_ctx with
    |Some state -> return ({rsp_ctx with resp_state_ctx = Some state})
    |None -> return ({rsp_ctx with resp_state_ctx = Some state})
  ) >>= function
    | Ok res -> return res
    | Error ex -> printf "%s\n%!" (Exn.to_string ex);
        return_resp_ctx (Some contexts.state_ctx) (Resp_Bad(None, "Bad Command")) None (** need to handle this TBD **)

(** need to add tag to the response as needed **)
let handle_response w context response =
  printf "handle_response\n%!";
  match context with 
  | Ok context -> write_resp w ~tag:context.request.tag response
  | Error e -> write_resp w response

let pr_state contexts = match contexts.state_ctx with
  |State_Notauthenticated -> Easy.logf `debug "in notauthenticated state\n%!"
  |State_Authenticated -> Easy.logf `debug "in authenticated state\n%!"
  |State_Selected -> Easy.logf `debug "in selected state\n%!"
  |State_Logout -> Easy.logf `debug "in logout state\n%!"

let update_contexts contexts context response =
  printf "update_contexts %d\n%!" (length contexts.req_ctx);
  match context with 
  |Ok (ctx) -> 
    let _ = pop contexts.req_ctx in 
    pushs contexts.req_ctx ctx
  |Error (e) -> contexts.req_ctx
*)

(* read a line from the network
 * if the line ends with literal {N} and it is not the append
 * then read N bytes, otherwise return the buffer
 *)
let rec read_network reader writer buffer =
  Easy.logf `debug "read_network\n%!";
  Lwt_io.read_line_opt reader >>= function
  | None -> return (`Ok (Buffer.contents buffer))
  | Some buff ->
  (** does command end in the literal {[0-9]+} ? **)
  let i = match_regex_i buff ~regx:"{\\([0-9]+\\)[+]?}$" in
  if i < 0 then (
    Easy.logf `debug "line not ending in literal\n%!";
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
    Easy.logf `debug "line is ending in literal %d %s --%s--\n%!" len literal sub;
    if match_regex ~case:false (Buffer.contents buffer) ~regx:append_regex ||
      match_regex ~case:false (Buffer.contents buffer) ~regx:lappend_regex then (
      Easy.logf `debug "handling append\n%!";
      Buffer.add_string buffer literal;
      Buffer.add_string buffer "\r\n";
      return (`Ok (Buffer.contents buffer))
    ) else if ((Buffer.length buffer) + len) > 10240 then (
      Easy.logf `debug "command size is too big: %s\n%!" (Buffer.contents buffer);
      return (`Bad)
    ) else (
      Easy.logf `debug "handling another command with the literal\n%!";
      (if match_regex literal ~regx:"[+]}$" = false then
        write_resp writer (Resp_Cont(""))
      else
        return ()
      ) >>
      let str = String.create len in
      Lwt.pick [
        Lwt_unix.sleep 5.0 >> return `Timeout; 
        Lwt_io.read_into_exactly reader str 0 len >> return (`Ok str)
      ] >>= function
      | `Ok str ->
        Buffer.add_string buffer str;
        read_network reader writer buffer
      | `Timeout ->
        Easy.logf `debug "network timeout\n%!";
        return (`Bad)
    )
  )

let parse_command buff =
  let open Parsing in
  let open Lexing in
  let open Lex in
  catch (fun () ->
    let lexbuff = Lexing.from_string buff in
    let command = (Parser.request (Lex.read (ref `Tag)) lexbuff) in
    Easy.logf `debug "get_request_context, returned from parser\n%!"; Out_channel.flush stdout;
    (* if done then need to match with idle 
     * if last was idle and any other command than done then error
     *)
    return (`Ok command)
  )
  (function 
  | SyntaxError e -> Easy.logf `debug "parse_command error %s\n%!" e; return (`Error (e))
  | Parser.Error -> Easy.logf `debug "parse_command bad command\n%!"; return (`Error ("bad command, parser"))
  | Interpreter.InvalidSequence -> return (`Error ("bad command, invalid sequence"))
  | Dates.InvalidDate -> return (`Error("bad command, invalid date"))
  | ExpectedDone -> return (`Error("Expected DONE"))
  | e -> return (`Error(Exn.backtrace()))
  )

let rec client_requests id (r,w) =
  catch ( fun () ->
  let buffer = Buffer.create 0 in
  read_network r w buffer >>= function
  | `Bad -> write_resp w (Resp_Bad(None, "")) >> client_requests id (r,w)
  | `Ok buff -> Easy.logf `debug "read buff --%s--\n%!" buff;
    parse_command buff >>= function
    | `Error e -> write_resp w (Resp_Bad(None,e)) >> client_requests id (r,w)
    | `Ok command -> write_resp w (Resp_Ok(None,"completed")) >> client_requests id (r,w)
  )
  (fun _ -> return `Done)
