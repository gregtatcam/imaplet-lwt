open Lwt
open Imaplet
open Commands
open Commands.Gitl

let input msg =
  Printf.printf "%s%!" msg;
  Lwt_io.read_line Lwt_io.stdin

let () =
  Lwt_main.run (
    let repo = Sys.argv.(1) in
    Gitl.create ~repo >>= fun t ->
      Printf.printf "%s\n%!" (Gitl.to_string t);
    let rec loop t =
      Lwt_io.read_line_opt Lwt_io.stdin >>= function
      | Some line ->
        if line = "" then
          loop t
        else
        catch (fun () ->
          let subs = Re.exec (Re_posix.compile_pat "^(help|read|find|head|update|log)( (.+))?$") line in
          match Re.get subs 1 with
          | "read" ->
            begin
            let sha = Sha.of_hex_string (Re.get subs 3) in
            read_object t sha >>= function
            |`Blob c -> Printf.printf "blob: %s\n%!" c; loop t
            |`Tree c -> Printf.printf "%s\n%!" (Tree.to_string c); loop t
            |`Commit c -> Printf.printf "%s\n%!" (Commit.to_string c); loop t
            |`Tag c -> Printf.printf "tag: %s\n%!" c; loop t
            | `None -> Printf.printf "failed to find\n%!"; loop t
            end
          | "find" ->
            begin
            let key = Key.of_unix (Re.get subs 3) in
            read_opt t key  >>= function
            | Some v -> Printf.printf "%s\n%!" v; loop t
            | None -> Printf.printf "not found\n%!"; loop t
            end
          | "head" ->
            Printf.printf "%s\n%!" (Gitl.to_string t); loop t
          | "update" ->
            let args = Re.get subs 3 in
            let subs = Re.exec (Re_posix.compile_pat "^([^ ]+) (.+)$") args in
            let key = Key.of_unix (Re.get subs 1) in
            let v = Re.get subs 2 in
            input "enter author: " >>= fun author ->
            input "enter commit message: " >>= fun message ->
            update_with_commit t ~author ~message key v >>= fun (_,t) ->
            loop t
          | "log" ->
            pretty_log t >>= fun l ->
            begin
            if l = [] then
              Printf.printf "repository is empty\n%!"
            else
              List.iter (Printf.printf "%s%!") l
            end;
            loop t
          | "help" ->
            Printf.printf "%s\n%!" (String.concat "\n" ["help - print list of commands";
              "read sha - read object referenced by sha";
              "find path - find sha referenced by unix path";
              "head - print the head info";
              "update path value - update/create value referenced by unix path";
              "log - print commit log"]); loop t
          | x -> Printf.printf "invalid command %s\n%!" x; loop t
        ) (function Not_found -> Printf.printf "invalid command or \
        sha\n%!";loop t|ex->Printf.printf "exception %s\n%!" (Printexc.to_string ex);loop t)
      | None -> return ()
    in
    loop t
  )
