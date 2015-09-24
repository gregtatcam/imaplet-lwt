open Lwt
open Re
open Imaplet
open Commands

exception InvalidCommand

let re = Re_posix.compile_pat "^([0-9]+) (.+)$" 
let re_read = Re_posix.compile_pat "^read ([0-9]+|\\*)$"
let re_fetch = Re_posix.compile_pat "^([^ ]+) fetch 1:([^ ]+)"
let re_login = Re_posix.compile_pat "^([^ ]+) login"
let re_select = Re_posix.compile_pat "^([^ ]+) select"

let get_tag re str =
  let subs = Re.exec re str in
  (subs,Re.get subs 1)

let imap str =
  if Re.execp re_fetch str then (
    let subs,tag = get_tag re_fetch str in
    `Fetch (tag,Re.get subs 2)
  ) else if Re.execp re_login str then (
    let _,tag = get_tag re_login str in
    `Login tag
  ) else if Re.execp re_select str then (
    let _,tag = get_tag re_select str in
    `Select tag
  ) else
    raise InvalidCommand

let opt_val = function
  | None -> raise InvalidCommand
  | Some v -> v

let rec args i repo port inflate =
  if i >= Array.length Sys.argv then
    repo, port, inflate
  else
    match Sys.argv.(i) with 
    | "-repo" -> args (i+2) (Some Sys.argv.(i+1)) port inflate
    | "-port" -> args (i+2) repo (Some (int_of_string Sys.argv.(i+1))) inflate
    | "-inflate" -> args (i+1) repo port true
    | _ -> raise InvalidCommand

let commands f =
  try 
    let repo,port,inflate = args 1 None None false in
    if repo = None || port = None then
      raise InvalidCommand;
    f (opt_val repo) (opt_val port) inflate
  with _ ->
    Printf.printf "usage: maildir_read -repo [repo location] -port [port number] -inflate\n%!"

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

let read_uidlst file num =
  Lwt_io.with_file file ~mode:Lwt_io.Input (fun ic ->
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

let readt = ref 0.
let writet = ref 0.

let timeit acc t =
  let t1 = Unix.gettimeofday() in
  acc := !acc +. (t1 -. t);
  t1

let sp = " "

let read_write_message file w cnt uncompress mutex =
  Lwt_io.with_file file ~mode:Lwt_io.Input (fun ic ->
    catch (fun () ->
      let t=Unix.gettimeofday() in
      Lwt_io.read ic >>= fun buff ->
      let t = timeit readt t in
      async (fun () ->
      let buff = if uncompress then (Imap_crypto.do_uncompress buff) else buff in
      let l = ["*"; sp;string_of_int cnt; sp; "FETCH"; sp; "("; "BODY[]"; sp; "{";string_of_int
        (String.length buff); "}";"\r\n";buff;")";"\r\n"] in
      let buff = String.concat "" l in
      async(fun() -> let t = Unix.gettimeofday () in
      Lwt_io.write w buff >>= fun () -> 
      timeit writet t;
      Lwt_mutex.unlock mutex; return());return());
      return ()
    ) (fun ex -> Printf.printf "exc: %s" (Printexc.to_string ex); return ())
  )

let read_files repo w num inflate =
  let uidlst_file = Filename.concat repo "imaplet.uidlst" in
  read_uidlst uidlst_file num >>= fun uids ->
  Lwt_list.fold_left_s (fun (i,acc) (uid,file) ->
      let m = Lwt_mutex.create () in
      Lwt_mutex.lock m;
      Printf.printf "%d\n%!" uid;
      let file = Filename.concat (Filename.concat repo "cur") file in
      read_write_message file w i inflate m >>= fun () ->
      return (i+1,m::acc)
  ) (1,[]) uids >>= fun (_,m's) ->
  List.iter Lwt_mutex.unlock m's;
  return ()

let () =
  commands (fun repo port inflate ->
    Lwt_main.run (
      server "0.0.0.0" port (fun r w ->
        Lwt_io.write w "CAPABILITY\r\n" >>= fun () ->
        let rec loop () =
          Lwt_io.read_line_opt r >>= function
          | Some l ->
            begin
            match (imap l) with
            | `Fetch (tag,num) ->
              begin
              try
                readt := 0.;
                writet := 0.;
                let t = Unix.gettimeofday () in
                read_files repo w num inflate >>= fun () ->
                Lwt_io.write w (tag ^ " ok\r\n") >>= fun () ->
                Printf.printf "total read: %.04f, write %.04f, time %.04f\n%!"
                !readt !writet (Unix.gettimeofday() -. t);
                loop ()
              with Not_found ->
                return ()
              end
            | `Login tag ->
              Lwt_io.write w (tag ^ " ok\r\n") >>= fun () ->
              loop ()
            | `Select tag ->
              Lwt_io.write w (tag ^ " ok\r\n") >>= fun () ->
              loop ()
            end
          | None -> return ()
        in
        loop ()
      )
    )
  )
