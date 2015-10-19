(*
 * Copyright (c) 2013-2016 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
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
open Commands.Gitl

module MapStr = Map.Make(String)

exception InvalidCommand

let rec args i repo compress =
  if i >= Array.length Sys.argv then
    repo,compress
  else
    match Sys.argv.(i) with 
    | "-repo" -> args (i+2) Sys.argv.(i+1) compress
    | "-compress" -> args (i+1) repo true
    | _ -> raise InvalidCommand

let commands f =
  try 
    let repo,compress = args 1 "" false in
    f repo compress
  with _ ->
    Printf.printf "usage: test_gitl -repo [repo] [-compress]\n%!"

let main repo compress =
  Lwt_main.run (
    let cache = ref MapStr.empty in
    let tree_sha = ref Sha.empty in
    Gitl.create ~repo ~compress ~cache () >>= fun t ->
      Printf.printf "%s\n%!" (Gitl.to_string t);
    let rec loop t =
      Lwt_io.read_line_opt Lwt_io.stdin >>= function
      | Some line ->
        if line = "" then
          loop t
        else
        catch (fun () ->
          let commands = "^(help|remove|rename|commit|read|find|head|update|log)( (.+))?$" in
          let re = Re_posix.compile_pat commands in
          let subs = Re.exec re line in
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
            update t key v >>= fun (_,sha) ->
            tree_sha := sha;
            loop t
          | "commit" ->
            let args = Re.get subs 3 in
            let subs = Re.exec (Re_posix.compile_pat "^([^ ]+) (.+)$") args in
            let author = Re.get subs 1 in
            let message = Re.get subs 2 in
            commit t !tree_sha ~author ~message >>= fun t ->
            loop t
          | "remove" ->
            let key = Key.of_unix (Re.get subs 3) in
            remove t key >>= fun sha ->
            tree_sha := sha;
            loop t
          | "update" ->
            let args = Re.get subs 3 in
            let subs = Re.exec (Re_posix.compile_pat "^([^ ]+) (.+)$") args in
            let src = Key.of_unix (Re.get subs 1) in
            let dest = Key.of_unix (Re.get subs 2) in
            rename t ~src ~dest >>= fun sha ->
            tree_sha := sha;
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
              "commit author message - commit updates";
              "remove path - remove object specified by unix path";
              "rename src dest - rename object specified by unix path";
              "log - print commit log"]); loop t
          | x -> Printf.printf "invalid command %s\n%!" x; loop t
        ) (function Not_found -> Printf.printf "invalid command or \
        sha\n%!";loop t|ex->Printf.printf "exception %s\n%!" (Printexc.to_string ex);loop t)
      | None -> return ()
    in
    loop t
  )

let () =
  commands main
