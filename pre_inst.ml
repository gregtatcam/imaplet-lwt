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

let backup dir fn ver =
  let src = path dir fn () in
  if exists src then ( 
    let dest = path dir (fn ^ ".back") ~ver () in
    Unix.rename src dest
  ) else
    ()

let () =
  let datadir = Sys.argv.(1) in
  let ver = Sys.argv.(2) in
  backup datadir "imaplet.cf" ver;
  backup datadir "users" ver
