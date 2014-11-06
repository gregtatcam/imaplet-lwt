(* 
 * 1 - bin dir
 * 2 - data dir
 *)
let () =
  let bin = Sys.argv.(1) in
  let data = Sys.argv.(2) in

  Printf.printf "  let data_path = \"%s/imaplet\"\n%!" data;
  Printf.printf "  let users_path = \"%s/imaplet/users\"\n%!" data;
  Printf.printf "  let config_path = \"%s/imaplet/imaplet.cf\"\n%!" data;
  Printf.printf "  let lmtp_srv_exec = \"%s/imaplet_lmtp\"\n%!" bin;
