open Lwt
open Imaplet_git

let () =
  Lwt_main.run (
    let repo = Sys.argv.(1) in
    Imaplet_git.create ~repo >>= fun t ->
      Printf.printf "%s\n%!" (Imaplet_git.to_string t);
    let rec loop () =
      Lwt_io.read_line_opt Lwt_io.stdin >>= function
      | Some line ->
        catch (fun () ->
          let subs = Re.exec (Re_posix.compile_pat "^(read|find) (.+)$") line in
          match Re.get subs 1 with
          | "read" ->
            begin
            let sha = Sha.of_string (Re.get subs 2) in
            read_object t sha >>= function
            |`Blob c -> Printf.printf "blob: %s\n%!" c; loop ()
            |`Tree c -> Printf.printf "%s\n%!" (Tree.to_string c); loop ()
            |`Commit c -> Printf.printf "%s\n%!" (Commit.to_string c); loop ()
            |`Tag c -> Printf.printf "tag: %s\n%!" c; loop ()
            end
          | "find" ->
            begin
            let key = Key.of_unix (Re.get subs 2) in
            read_opt t key  >>= function
            | Some v -> Printf.printf "%s\n%!" v; loop()
            | None -> Printf.printf "not found\n%!"; loop()
            end
          | _ -> Printf.printf "invalid command\n%!"; loop()
        ) (function Not_found -> Printf.printf "invalid command or \
        sha\n%!";loop()|ex->Printf.printf "exception %s\n%!" (Printexc.to_string ex);loop())
      | None -> return ()
    in
    loop ()
  )
