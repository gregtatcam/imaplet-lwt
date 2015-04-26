let path dir fn ?ver () =
  let ver =
    match ver with
    | None -> ""
    | Some ver -> "." ^ ver
  in
  Filename.concat dir (Filename.concat "imaplet" (fn ^ ver))

let exists fn =
  try
    let st = Unix.stat fn in
    st.Unix.st_kind = Unix.S_REG
  with _ -> false

let check dir fn ver rm =
  let src = path dir fn ~ver () in
  if exists src then (
    let src = path dir (fn ^ ".back") ~ver () in
    if exists src then (
      let dest = path dir fn () in
      Unix.rename src dest
    )
  ) else (
    let src = path dir (fn ^ ".back") ~ver () in
    if exists src then (
      let dest = path dir fn () in
      let temp = path dir (fn ^ "_back") () in
      let dest1 = path dir fn ~ver () in
      Unix.rename dest temp;
      Unix.rename src dest;
      begin
      try
        let nul = if (String.sub rm 0 3) = "del" then " 2> nul" else " 2> /dev/null" in
        let cmd = rm ^ " " ^ dest ^ ".*" ^ nul in
        let _ = Unix.system cmd in ()
      with _ -> ()
      end;
      Unix.rename temp dest1
    )
 )

let () =
  let datadir = Sys.argv.(1) in
  let ver = Sys.argv.(2) in
  let rm = Sys.argv.(3) in
  check datadir "imaplet.cf" ver rm;
  check datadir "users" ver rm
