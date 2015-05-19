open Lwt
open Imaplet
open Commands

exception InvalidCommand

let rec args i srv port intf =
  if i >= Array.length Sys.argv then
    srv,port,intf
  else
    match Sys.argv.(i) with 
    | "-s" -> args (i+2) (Some Sys.argv.(i+1)) port intf
    | "-p" -> args (i+2) srv (int_of_string Sys.argv.(i+1)) intf
    | "-i" -> args (i+2) srv port (Some Sys.argv.(i+1))
    | _ -> raise InvalidCommand

let usage () =
  Printf.printf "usage: stun -s [stun server] -p [port] -i [interface ip]\n%!"

let commands f =
  try 
    let srv,port,intf = args 1 None 3478 None in
      try 
        f srv port intf
      with ex -> Printf.printf "%s\n" (Printexc.to_string ex)
  with _ -> usage ()

let get_stun_ip srv =
  let srv = match srv with |Some srv -> srv | None -> "stun.l.google.com" in
  if try let _ = Unix.inet_addr_of_string srv in true with _ -> false then
    return (Some srv)
  else (
    Imaplet_dns.gethostbyname srv >>= fun ips ->
    if ips <> [] then
      return (Some (List.hd ips))
    else
      return None
  )

let () =
  commands (fun srv port intf ->
  Lwt_main.run (
    catch (fun () ->
      get_stun_ip srv >>= function
      | Some ip ->
        begin
        Imaplet_stun.stun_request ?interface:intf ip port >>= function
        | Some (addr,port) -> Printf.printf "%s %d\n%!" addr port; return ()
        | None -> Printf.printf "no mapped address present in the response\n%!"; return ()
        end
      | None -> Printf.printf "can't resolve stun server domain name\n%!"; return ()
    )
    (fun ex -> Printf.printf "%s\n%!" (Printexc.to_string ex);return())
  ))
