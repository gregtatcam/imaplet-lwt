open Lwt

exception InvalidCommand

let get_user str = 
  try
    let re = Re_posix.compile_pat ~opts:[`ICase] "^([^:]+):(.+)$" in
    let subs = Re.exec re str in
    let user = Re.get subs 1 in
    let pswd = Re.get subs 2 in
    (user,pswd)
  with Not_found -> raise InvalidCommand

let rec args i addr port user number =
  if i >= Array.length Sys.argv then
    addr,port,user,number
  else
    match Sys.argv.(i) with 
    | "-address" -> args (i+2) Sys.argv.(i+1) port user number
    | "-port" -> args (i+2) addr (int_of_string (Sys.argv.(i+1))) user number
    | "-user" -> args (i+2) addr port (get_user Sys.argv.(i+1)) number
    | "-number" -> args (i+2) addr port user Sys.argv.(i+1)
    | _ -> raise InvalidCommand

let usage () =
  Printf.fprintf stderr "usage: fetch_client -address [address] \
  -port [port] -user [user:pswd] -number [number]\n%!"

let commands f =
  try 
    let addr,port,(user,password),number = args 1 "127.0.0.1" 143 ("","") "*" in
      try 
        f addr port user password number
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

let re_ok = Re_posix.compile_pat ~opts:[`ICase] "^a001 ok"

let read_response ic re =
  let rec read_ () =
    Lwt_io.read_line ic >>= fun line ->
    if Re.execp ~pos:0 re line then
      return ()
    else
      read_ ()
  in
  read_()

let () =
  commands (fun addr port user password number ->
    Lwt_main.run (catch(fun() ->
        let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string addr,port) in
        Lwt_io.with_connection sockaddr (fun (ic,oc) ->
          Lwt_io.read_line ic >>= fun _ -> (* server capabilities greeting *)
          Lwt_io.write oc (String.concat "" ["a001 login ";user;" ";password;"\r\n"]) >>= fun () ->
          read_response ic re_ok >>= fun () -> (* login response *)
          Lwt_io.write oc "a001 select inbox\r\n" >>= fun () ->
          read_response ic re_ok >>= fun () -> (* select response *)
          Lwt_io.write oc (String.concat "" ["a001 fetch 1:";number; " body[]\r\n"]) >>= fun () ->
          let t = Unix.gettimeofday () in
          read_response ic re_ok >>= fun () ->
          Printf.printf "time: %.04f\n%!" (Unix.gettimeofday() -. t);
          return ()
        )
      )
      (fun ex -> Printf.fprintf stderr "client: fatal exception: %s %s"
        (Printexc.to_string ex) (Printexc.get_backtrace()); return()
      )
    )
  )
