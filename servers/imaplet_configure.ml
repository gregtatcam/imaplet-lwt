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
open Imaplet
open Commands
open Server_config

exception Done
exception SystemCallFailed of string
exception AccountExists

module MapStr = Map.Make(String)

let yn = [("y",(fun () -> return true));("n",(fun() -> return false));]
let yn_exn = [("y",(fun () -> return ()));("n",(fun() -> raise Done));]
let ync = [("y",(fun () -> return true));("n",(fun() -> return false));("c",(fun()->raise Done))]

let confirm msg l =
  Printf.printf "%s%!" msg;
  let rec conf l =
    Lwt_io.read_line Lwt_io.stdin >>= fun res ->
    try
      let (_,f) = List.find (fun (r,_) -> r = (String.lowercase res)) l in
      f ()
    with Not_found -> Printf.printf "Invalid input, try again:%!"; conf l
  in
  conf l

let input msg regx =
  Printf.printf "%s%!" msg;
  let rec _input () =
    Lwt_io.read_line Lwt_io.stdin >>= fun res ->
    if Regex.match_regex ~regx res then
      return res
    else (
      Printf.printf "Invalid input, try again:%!"; _input ()
    )
  in
  _input ()

let system cmd =
  Lwt_unix.system cmd >>= function
  | WEXITED i -> return (if i = 0 then () else raise (SystemCallFailed cmd))
  | _ -> raise (SystemCallFailed cmd)

let dir f =
  Utils.exists f Unix.S_DIR

let check_users user_path user pswd =
  catch (fun () ->
  dir user_path >>= fun res ->
  if res then raise AccountExists;
  Utils.lines_of_file srv_config.users_path ~init:() ~f:(fun line _ ->
    if Regex.match_regex ~case:false ~regx:("^" ^ user ^ ":") line then
      raise AccountExists;
    return ()
  ) >> return false) (fun _ -> return true)


let get_interfaces () =
  Lwt_unix.gethostname () >>= fun host ->
  Lwt_unix.gethostbyname host >>= fun res ->
  let intfs =Array.fold_left (fun acc addr -> 
    (Unix.string_of_inet_addr addr) :: acc
  ) ["127.0.0.1";"0.0.0.0"] res.h_addr_list in
  let (_,intfs_str,conf) = List.fold_right (fun addr (i,acc,conf) -> 
    (i+1,Printf.sprintf "%s%d(%s):" acc i addr,(string_of_int i, fun () ->
      return addr) :: conf)
  ) intfs (1,"Choose interface:",[]) in
  return (intfs,intfs_str,conf)

let bool_of_yn = function
  | "Y"|"y"->true
  |_->false

let bool_to_ed b =
  if b then
    "enable"
  else
    "disable"

(* configure the server for the first run *)
let () = Lwt_main.run (
  catch (fun () ->
  let config = srv_config in
  Printf.printf "--- Configuring imaplet server.
Current configuration will be saved in %s.back.\n%!" Install.config_path;
  confirm "Do you want to proceed?\nY(yes)|N(no):" yn_exn >>= fun _ ->
  confirm "\n--- User email accounts will be stored in
/var/mail/accounts\nY(yes)|N(different location)|C(cancel):" ync >>= fun res ->
  begin
  if res then
    return "/var/mail/accounts"
  else (
    input "Enter another location:" ""
  )
  end >>= fun account_location ->
  let config = {config with irmin_path = account_location ^ "/%user%/repo";
    user_cert_path = account_location ^ "/%user%/cert"} in
  system ("mkdir -p " ^ account_location) >>= fun () ->
  Printf.printf "\n--- Configuring IMAP Server.\n";
  Printf.printf "The following configuration should support the most common cases\n%!";
  Printf.printf "Interface to accept client connections on: 0.0.0.0 (any)\n%!";
  Printf.printf "Port to accept client connections on: 993 (default SSL port)\n%!";
  Printf.printf "SSL/STARTTLS: enabled\n%!";
  confirm "Do you want to proceed?\nY(yes)|N(different configuration)|C(cancel):" ync >>= fun res ->
  begin
    if res then
      return ("0.0.0.0",993,true,true)
    else (
      get_interfaces () >>= fun (intfs,intfs_str,conf) ->
      confirm intfs_str conf >>= fun addr ->
      input "Enter port:" "^[1-9]+" >>= fun port ->
      input "SSL enabled Y(yes)|N(no):" "^[YyNn]$" >>= fun ssl ->
      input "STARTTLS enabled Y(yes)|N(no):" "^[YyNn]$" >>= fun starttls ->
      return (addr,int_of_string port,bool_of_yn ssl, bool_of_yn starttls)
    )
  end >>= fun (addr,port,ssl,starttls) ->
  let config = {config with addr;port;ssl;starttls} in
  Printf.printf "\n--- Configuring SMTP Server.\n";
  Printf.printf "The following configuration should support the most common cases\n%!";
  Printf.printf "Interface to accept client connections on: 0.0.0.0 (any)\n%!";
  Printf.printf "Ports to accept client connections on: 25,587\n%!";
  Printf.printf "SSL(uncommon email client support): disabled\n%!";
  Printf.printf "STARTTLS: enabled\n%!";
  confirm "Do you want to proceed?\nY(yes)|N(different configuration)|C(cancel):" ync >>= fun res ->
  let get_ports ports =
    List.fold_left (fun acc p -> (int_of_string p) :: acc) [] (Str.split
    (Str.regexp ",") ports)
  in
  begin
    if res then
      return ("0.0.0.0",[25;587],false,true)
    else (
      get_interfaces () >>= fun (intfs,intfs_str,conf) ->
      confirm intfs_str conf >>= fun addr ->
        input "Enter port:" "^[0-9]+\\(,[0-9]+\\)*$" >>= fun ports ->
      input "STARTTLS enabled Y(yes)|N(no):" "^[YyNn]$" >>= fun starttls ->
      input "SSL enabled N(no)|Y(yes):" "^[YyNn]$" >>= fun ssl ->
      return (addr,get_ports ports, bool_of_yn ssl, bool_of_yn starttls)
    )
  end >>= fun (smtp_addr,smtp_port,smtp_ssl,smtp_starttls) ->
  let config = {config with smtp_addr;smtp_port;smtp_ssl;smtp_starttls} in
  Printf.printf "\n--- Configuring error logging location\n%!";
  Printf.printf "By default the errors and information are logged into /var/log/[imaplet.log,smtplet.log]\n%!";
  Printf.printf "Logging into the files is useful when the imaplet server runs as the daemon process\n%!";
  Printf.printf "If the imaplet server runs from command line then it might be more useful to log the messages to the terminal\n%!";
  begin
  input "Log the messages to 1(/var/log):2(different location):3(stderr):" "^[1-3]$" >>= function
  | "1" -> system ("mkdir -p /var/log") >> return "/var/log"
  | "2" -> (input "Enter location:" "") >>= fun dir ->
    system ("mkdir -p " ^ dir) >> return dir
  | "3" -> return "stderr"
  | _ -> return "stderr"
  end >>= fun log ->
  let config = {config with log} in
  Printf.printf "\n--- Configuring logging level\n%!";
  Printf.printf "Imaplet server can log errors only(less disk space),different
level of detailed information(more disk space), or debug information(most disk
space)\n%!";
  begin
  input "1(error):2(info1):3(info2):4(info3):5(debug):" "^[1-5]$" >>= function
  | "1" -> return (`Error,"error")
  | "2" -> return (`Info1,"info1")
  | "3" -> return (`Info2,"info2")
  | "4" -> return (`Info3,"info3")
  | "5" -> return (`Debug,"debug")
  | _ -> return (`Error,"error")
  end >>= fun (log_level,level_str) ->
  let ports_to_str ports =
    List.fold_left (fun acc p -> if acc = "" then (string_of_int p) else
      (string_of_int p) ^ "," ^ acc) "" ports
  in
  let config = {config with log_level} in
  Printf.printf "\n### Entered configuration:\n%!";
  Printf.printf "Account location: %s\n%!" account_location;
  Printf.printf "IMAP interface: %s\n%!" config.addr;
  Printf.printf "IMAP port: %d\n%!" config.port;
  Printf.printf "IMAP SSL: %s\n%!" (bool_to_ed config.ssl);
  Printf.printf "IMAP STARTTLS: %s\n%!" (bool_to_ed config.starttls);
  Printf.printf "SMTP interface: %s\n%!" config.smtp_addr;
  Printf.printf "SMTP ports: %s\n%!" (ports_to_str config.smtp_port);
  Printf.printf "SMTP SSL: %s\n%!" (bool_to_ed config.smtp_ssl);
  Printf.printf "SMTP STARTTLS: %s\n%!" (bool_to_ed config.smtp_starttls);
  Printf.printf "Logging location: %s\n%!" config.log;
  Printf.printf "Logging level: %s\n%!" level_str;
  confirm "### Do you want to proceed?\nY(yes)|N(no):" yn_exn >>= fun _ ->
  let config_tmp = Install.config_path ^ ".tmp" in
  Lwt_io.with_file ~mode:Lwt_io.Input Install.config_path (fun ic ->
    Lwt_io.with_file ~mode:Lwt_io.Output config_tmp (fun oc ->
      let map = MapStr.empty in
      let map = MapStr.add "^irmin_path" ("irmin_path " ^ config.irmin_path) map in
      let map = MapStr.add "^user_cert_path" ("user_cert_path " ^ config.user_cert_path) map in
      let map = MapStr.add "^addr" ("addr " ^ config.addr) map in
      let map = MapStr.add "^port" ("port " ^ (string_of_int config.port)) map in
      let map = MapStr.add "^ssl" ("ssl " ^ (string_of_bool config.ssl)) map in
      let map = MapStr.add "^starttls" ("starttls " ^ (string_of_bool config.starttls)) map in
      let map = MapStr.add "^smtp_addr" ("smtp_addr " ^ config.smtp_addr) map in
      let map = MapStr.add "^smtp_port" ("smtp_port " ^ (ports_to_str config.smtp_port)) map in
      let map = MapStr.add "^smtp_ssl" ("smtp_ssl " ^ (string_of_bool config.smtp_ssl)) map in
      let map = MapStr.add "^smtp_starttls" ("smtp_starttls " ^ (string_of_bool config.smtp_starttls)) map in
      let map = MapStr.add "^log" ("log " ^ config.log) map in
      let map = MapStr.add "^log_level" ("log_level " ^ level_str) map in
      let rec read map =
        Lwt_io.read_line_opt ic >>= function
        | None -> return map
        | Some str ->
          let (str,map) =
          MapStr.fold (fun k v (str,map) -> 
            if Regex.match_regex ~regx:k str then (
              (v,map)
            ) else
              (str,MapStr.add k v map)
          ) map (str,MapStr.empty) 
          in
          Lwt_io.write_line oc str >>
          read map
      in
      read map >>= fun map -> 
      MapStr.fold (fun _ v acc ->
        acc >>
        Lwt_io.write_line oc v
      ) map (return())
    )
  ) >>
  let echo_on on =
    Lwt_unix.tcgetattr Lwt_unix.stdin >>= fun io ->
    let io = {io with c_echo = on} in
    Lwt_unix.tcsetattr Lwt_unix.stdin TCSANOW io
  in
  system ("mv " ^ Install.config_path ^ " " ^ Install.config_path ^ ".back") >>
  system ("mv " ^ config_tmp ^ " " ^ Install.config_path) >>= fun () ->
  Printf.printf "--- Updated imaplet configuration\n%!";
  confirm "\n--- Would you like to add an email user account? Y(yes)|N(no):" yn_exn >>= fun _ ->
  input "\nEnter user name:" "" >>= fun user ->
  echo_on false >>
  let rec password () =
    input "\nEnter password:" "" >>= fun p ->
    input "\nRe-enter password:" "" >>= fun p1 ->
    if p = p1 then
      return p
    else (
      Printf.printf "Passwords don't match\n%!";
      password ()
    )
  in
  password () >>= fun password ->
  echo_on true >>= fun () ->
  let user_path = Filename.concat account_location user in
  check_users user_path user password >>= fun exists ->
  begin
    if exists then (
      confirm "### User account already exists. Overwriting the account will delete all
emails! Do you want to overwrite it? Y(yes)|N(no):" yn_exn >>
      return " -f"
    ) else
      return ""
  end >>= fun force ->
  system ("imaplet_create_account -u " ^ user ^ ":" ^ password ^ force) 
  ) (function
  | Done -> return ()
  | ex -> Printf.printf "Exception: %s" (Printexc.to_string ex); return ()
  )
)
