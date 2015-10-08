open Lwt

exception InvalidCommand of string

let rec args i addr port num =
  if i >= Array.length Sys.argv then
    addr,port,num
  else
    match Sys.argv.(i) with 
    | "-addr" -> args (i+2) Sys.argv.(i+1) port num
    | "-port" -> args (i+2) addr (int_of_string Sys.argv.(i+1)) num
    | "-num" -> args (i+2) addr port (int_of_string Sys.argv.(i+1))
    | s -> raise (InvalidCommand s)

let commands f =
  try
    let addr,port,num =
      args 1 "127.0.0.1" 20001 5 in
    f addr port num
  with InvalidCommand x -> Printf.printf "usage: irvsfsio_cl -addr [server \
    address] -port [server port] -num [repeat]\n";
  raise (InvalidCommand x)

let addr_inet _addr _port = (Unix.ADDR_INET (Unix.inet_addr_of_string _addr,_port))

let read_response r =
  catch (fun () ->
  let rec _read () =
    Lwt_io.read r ~count:(2048*5) >>= fun buff ->
    if buff = "" then (
      return ()
    ) else (
      _read ()
    )
  in
  _read ()
  ) (fun _ -> return ())

let command addr num cmd =
  Lwt_io.with_connection addr (fun (r,w) ->
    Printf.printf "connected to the server, requesting %s\n%!" cmd;
    Lwt_io.write_line w cmd >>= fun () ->
    let t = Unix.gettimeofday () in
    read_response r >>= fun () ->
    Printf.printf "total read %.04f\n%!" (Unix.gettimeofday () -. t);
    return ()
  )

let crt_cmd l cmd num = 
  let rec crt i l =
    if i > num then
      l
    else
      crt (i+1) (cmd :: l)
  in
  crt 1 l

let () =
  commands (fun _addr _port _num ->
  Lwt_main.run (
    Printf.printf "started client\n%!";
    let cmds = crt_cmd [] "file" _num in
    let cmds = crt_cmd cmds "irmin" _num in
    let cmds = List.rev ("quit" :: cmds) in
    Lwt_list.iter_s (command (addr_inet _addr _port) _num) cmds
  )
  )
