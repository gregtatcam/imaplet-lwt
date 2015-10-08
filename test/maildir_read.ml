open Lwt
open Re
open Irmin_unix
open Sexplib
open Sexplib.Conv
open Imaplet
open Commands

exception InvalidCommand

let re = Re_posix.compile_pat "^([0-9]+) (.+)$" 
let re_read = Re_posix.compile_pat "^read ([0-9]+|\\*)$"
let re_fetch = Re_posix.compile_pat "^([^ ]+) fetch 1:([^ ]+)"
let re_login = Re_posix.compile_pat "^([^ ]+) login ([^ ]+)"
let re_select = Re_posix.compile_pat "^([^ ]+) select ([^ ]+)"

(*
 mutest1000r:{SHA256}iBNo0571Fbtry25GuXs230leItrk6KxO16fVd73o057OKP704=::::/var/mail/accounts/mutest1000r/repo:maildir:af:ef:cff:sf:hf:mf
 *)

let login user =
  let users_db = "/usr/local/share/imaplet/users" in
  Lwt_io.with_file ~flags:[Unix.O_NONBLOCK] ~mode:Lwt_io.Input users_db (fun ic ->
    let rec read () =
      Lwt_io.read_line_opt ic >>= function
      | Some line ->
        begin
        let re = String.concat "" ["^"; user; ":.+:([^:]+):(irmin|maildir):a[tf]:e[tf]:c([tf])[tf]:sf:hf:mf$"] in
        try
          let subs = Re.exec (Re_posix.compile_pat re) line in
          return (Some (Re.get subs 1, Re.get subs 2, if (Re.get subs 3) = "t" then true else false))
        with Not_found -> read()
        end
      | None -> return None
    in
    read ()
  )

let get_tag re str =
  let subs = Re.exec re str in
  (subs,Re.get subs 1)

let imap str =
  if Re.execp re_fetch str then (
    let subs,tag = get_tag re_fetch str in
    return (`Fetch (tag,Re.get subs 2))
  ) else if Re.execp re_login str then (
    let subs,tag = get_tag re_login str in
    login (Re.get subs 2) >>= function
    | Some (repo,store,inflate) -> return (`Login (tag,Re.get subs 2,repo,store,inflate))
    | None -> return (`Error tag)
  ) else if Re.execp re_select str then (
    let subs,tag = get_tag re_select str in
    return (`Select (tag,Re.get subs 2))
  ) else
    raise InvalidCommand

let opt_val = function
  | None -> raise InvalidCommand
  | Some v -> v

let rec args i port =
  if i >= Array.length Sys.argv then
    port
  else
    match Sys.argv.(i) with 
    | "-port" -> args (i+2) (Some (int_of_string Sys.argv.(i+1)))
    | _ -> raise InvalidCommand

let commands f =
  try 
    let port = args 1 None in
    if port = None then
      raise InvalidCommand;
    f (opt_val port)
  with _ ->
    Printf.printf "usage: maildir_read -port [port number]\n%!"

let try_close cio =
  catch (fun () -> Lwt_io.close cio)
  (function _ -> return ())

let try_close_sock sock =
  catch (fun () -> Lwt_unix.close sock)
  (function _ -> return ())

let init_socket ?(stype=Unix.SOCK_STREAM) addr port =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
  let socket = Lwt_unix.socket Unix.PF_INET stype 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.set_blocking socket false;
  Lwt_unix.bind socket sockaddr;
  return socket

let create_srv_socket addr port =
  init_socket addr port >>= fun socket ->
  Lwt_unix.listen socket 10;
  return socket

let accept_cmn sock =
  Lwt_unix.accept sock >>= fun (sock_c, addr) ->
  let ic = Lwt_io.of_fd ~close:(fun()->return()) ~mode:Lwt_io.input sock_c in
  let oc = Lwt_io.of_fd ~close:(fun()->return()) ~mode:Lwt_io.output sock_c in
  return (sock_c,(ic,oc))

let server addr port f =
  create_srv_socket addr port >>= fun sock ->
  let rec connect f sock =
    accept_cmn sock >>= fun (sock_c,(netr,netw)) ->
    async ( fun () ->
      catch( fun () ->
        f netr netw >>= fun () ->
        try_close netr >>= fun () -> try_close netw >>= fun () -> try_close_sock sock_c
      ) 
      (fun ex -> try_close netr >>= fun () -> try_close netw >>= fun () -> try_close_sock sock_c
      >>= fun () -> Printf.printf "exception %s\n%!" (Printexc.to_string ex);
      return ()
      )
    ); connect f sock 
  in
  connect f sock 

let readt = ref 0.
let writet = ref 0.
let comprt = ref 0.

let timeit acc t =
  let t1 = Unix.gettimeofday() in
  acc := !acc +. (t1 -. t);
  t1

let sp = " "

module type Maildir_intf =
sig
  type t

  val create : user:string -> repo:string -> mailbox:string -> t Lwt.t

  val read_index: t -> num:string -> (int * string) list Lwt.t

  val read_message: t -> id:string -> string Lwt.t
end

module MaildirFile : Maildir_intf =
struct
  type t = {user:string; repo:string; mailbox:string}

  let get_dir t =
    if String.lowercase t.mailbox = "inbox" then
      t.repo
    else
      Filename.concat t.repo ("." ^ t.mailbox)

  let create ~user ~repo ~mailbox =
    return {user;repo;mailbox}

  let read_index t ~num =
    let file = Filename.concat (get_dir t) "imaplet.uidlst" in
    Lwt_io.with_file ~flags:[Unix.O_NONBLOCK] ~mode:Lwt_io.Input file (fun ic ->
      let rec read uids =
        if num = "*" || (int_of_string num) > (List.length uids) then (
          Lwt_io.read_line_opt ic >>= function
          | Some l ->
            let subs = Re.exec re l in
            let uid = int_of_string (Re.get subs 1) in
            let file = Re.get subs 2 in
            read ((uid,file) :: uids)
          | None -> return (List.rev uids)
        ) else
          return (List.rev uids)
      in
      read [] 
    )

  let read_message t ~id =
    let file = Filename.concat (Filename.concat (get_dir t) "cur") id in
    Lwt_io.with_file ~flags:[Unix.O_NONBLOCK] ~mode:Lwt_io.Input file (fun ic ->
      Lwt_io.read ic
    )
end

module MaildirIrmin : Maildir_intf =
struct
  module Store = Irmin.Basic (Irmin_git.FS) (Irmin.Contents.String)
  module View = Irmin.View(Store)

  type t = {user:string; repo:string; mailbox:string; store:(string -> View.db)}

  let task msg =
    let date = Int64.of_float (Unix.gettimeofday ()) in
    let owner = "imaplet <imaplet@openmirage.org>" in
    Irmin.Task.create ~date ~owner msg

  let create_store repo =
    let _config = Irmin_git.config
      ~root:repo
      ~bare:true () in
    Store.create _config task

  let create ~user ~repo ~mailbox =
    create_store repo >>= fun store ->
    return {user;repo;mailbox;store}

  let read_index t ~num =
    let mailbox = if (String.lowercase t.mailbox = "inbox") then "INBOX" else t.mailbox in
    let key = ["imaplet"; t.user; "mailboxes"; mailbox; "index"] in
    Store.read_exn (t.store "reading") key >>= fun index_sexp_str ->
    return (list_of_sexp
    (fun i ->
      (* change to same as maildir *)
      let (file,uid) =
      pair_of_sexp (fun a -> Sexp.to_string a) (fun b -> int_of_string (Sexp.to_string b)) i in
      (uid,file)
    ) (Sexp.of_string index_sexp_str))

  let read_message t ~id =
    let key = ["imaplet"; t.user; "storage";id] in
    Store.read_exn (t.store "reading") key
end

module type MaildirReader_intf =
sig
  val read_files : user:string -> repo:string -> mailbox:string -> writer:Lwt_io.output_channel -> num:string ->
    inflate:bool -> unit Lwt.t
end

module MakeMaildirReader(M:Maildir_intf) : MaildirReader_intf =
struct
  let write_messages strm w uncompress =
    let rec _write d uid = 
      Lwt_stream.get strm >>= function
      | Some message -> 
        Printf.printf "writing data %d, delay %.04f\n%!" uid (Unix.gettimeofday () -. d);
        let t=Unix.gettimeofday() in
        let message = if uncompress then (Imap_crypto.do_uncompress message) else message in
        let l = ["*"; sp;string_of_int uid; sp; "FETCH"; sp; "("; "BODY[]"; sp; "{";string_of_int
          (String.length message); "}";"\r\n";message;")";"\r\n"] in
        let buff = String.concat "" l in
        let t = timeit comprt t in
        Lwt_io.write w buff >>= fun () -> 
        let _ = timeit writet t in
        _write (Unix.gettimeofday()) (uid+1)
      | None -> return ()
    in
    _write (Unix.gettimeofday()) 1
  
  let read_files ~user ~repo ~mailbox ~writer ~num ~inflate =
    let (strm,push_strm) = Lwt_stream.create () in
    M.create ~user ~repo ~mailbox >>= fun maildir ->
    M.read_index maildir ~num >>= fun uids ->
    Lwt_list.iter_p (fun f -> f()) 
    [
      (
      (fun () ->
      Lwt_list.iter_s (fun (_,file) ->
        let t = Unix.gettimeofday () in
        M.read_message maildir ~id:file >>= fun message ->
        let _ = timeit readt t in
        push_strm (Some message);
        Lwt_unix.yield ()
      ) uids >>= fun () ->
      push_strm None;
      return ()
      )
      );
      (
      (fun () ->
      write_messages strm writer inflate
      )
      )
    ]
end

let () =
  commands (fun port ->
    Lwt_main.run (
      server "0.0.0.0" port (fun r w ->
        Lwt_io.write w "CAPABILITY\r\n" >>= fun () ->
        let rec loop ?(user="") ?(repo="") ?(store="") ?(mailbox="") ?(inflate=false) () =
          Lwt_io.read_line_opt r >>= function
          | Some l ->
            begin
            imap l >>= function
            | `Fetch (tag,num) ->
              begin
              try
                readt := 0.;
                writet := 0.;
                comprt := 0.;
                let t = Unix.gettimeofday () in
                begin
                if store = "maildir" then (
                  let module MaildirReader = MakeMaildirReader(MaildirFile) in
                  MaildirReader.read_files ~user ~repo ~mailbox ~writer:w ~num ~inflate
                ) else (
                  let module MaildirReader = MakeMaildirReader(MaildirIrmin) in
                  MaildirReader.read_files ~user ~repo ~mailbox ~writer:w ~num ~inflate
                )
                end >>= fun () ->
                Lwt_io.write w (tag ^ " ok\r\n") >>= fun () ->
                Printf.printf "total read: %.04f, write %.04f, compress %.04f, time %.04f\n%!"
                !readt !writet !comprt (Unix.gettimeofday() -. t);
                loop ()
              with Not_found ->
                return ()
              end
            | `Login (tag,user,repo,store,inflate) ->
              Lwt_io.write w (tag ^ " ok\r\n") >>= fun () ->
              loop ~user ~repo ~store ~inflate ()
            | `Select (tag,mailbox) ->
              Lwt_io.write w (tag ^ " ok\r\n") >>= fun () ->
              loop ~user ~repo ~store ~mailbox ~inflate ()
            | `Error tag ->
              Lwt_io.write w (tag ^ " bad\r\n") >>= fun () ->
              loop ()
            end
          | None -> return ()
        in
        loop ()
      )
    )
  )
