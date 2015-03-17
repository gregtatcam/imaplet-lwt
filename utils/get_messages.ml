open Lwt
open Imaplet
open Commands

let get_messages path start stop =
  Lwt_io.with_file ~mode:Lwt_io.Input path (fun ic ->
    let buffer = Buffer.create 10000 in
    let rec loop ic buffer cnt = 
      let read ic cnt stop =
        if cnt > stop then
          return None
        else
          Lwt_io.read_line_opt ic 
      in
      read ic cnt stop >>= function
      | Some line ->
      let line = line ^ "\n" in
      let regx = "^from [^ ]+ " ^ Regex.dayofweek ^ " " ^ Regex.mon ^ " " ^
        "[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [0-9]+" in
      if Regex.match_regex line ~case:false ~regx && Buffer.length buffer > 0 then (
        if cnt >= start then
          Printf.printf "%s%!" (Buffer.contents buffer);
        Buffer.clear buffer;
        Buffer.add_string buffer line;
        loop ic buffer (cnt+1)
      ) else (
        Buffer.add_string buffer line;
        loop ic buffer cnt
      )
      | None -> return ()
    in
    loop ic buffer 1 
  )

let () =
  Lwt_main.run (
    let path = Sys.argv.(1) in
    let _ = Regex.match_regex Sys.argv.(2) ~regx:"\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?" in
    let start = int_of_string (Str.matched_group 1 Sys.argv.(2)) in
    let stop = 
      try
        let stop = Str.matched_group 3 Sys.argv.(2) in
        int_of_string stop
      with _ -> start
    in
    get_messages path start stop
  )
