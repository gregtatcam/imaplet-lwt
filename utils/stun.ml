open Lwt
open Imaplet
open Commands

exception InvalidCommand

let rec args i srv intf =
  if i >= Array.length Sys.argv then
    srv,intf
  else
    match Sys.argv.(i) with 
    | "-s" -> args (i+2) (Some Sys.argv.(i+1)) intf
    | "-i" -> args (i+2) srv (Some Sys.argv.(i+1))
    | _ -> raise InvalidCommand

let usage () =
  Printf.printf "usage: stun -s [stun server] -i [interface ip]\n%!"

let commands f =
  try 
    let srv,intf = args 1 None None in
      try 
        f srv intf
      with ex -> Printf.printf "%s\n" (Printexc.to_string ex)
  with _ -> usage ()

let () =
  commands (fun srv intf ->
  Lwt_main.run (
    Printf.printf "starting request\n%!";
    let srv = match srv with |Some srv -> srv | None -> "stun.l.google.com" in
    Imaplet_dns.gethostbyname srv >>= fun ips ->
    catch (fun () ->
    Imaplet_stun.stun_request ?interface:intf (List.hd ips) 19302 >>= fun (addr,port) ->
    Printf.printf "%s %d\n%!" addr port;
    return ())
    (fun ex -> Printf.printf "%s\n%!" (Printexc.to_string ex);return())
  ))
