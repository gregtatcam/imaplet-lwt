open Core_replace

let ok_or_stderr res ?line ~on_error =
  match res with
  | Result_.Ok a -> a
  | Result_.Error b ->
    begin
    match line with
    | Some n -> Printf.eprintf "Line %d: " n
    | None   -> ()
    end;
    Printf.eprintf "%s\n%!" b;
    on_error

