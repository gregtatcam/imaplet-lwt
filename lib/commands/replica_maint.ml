(*
 * Copyright (c) 2013-2015 Gregory Tsipenyuk <gt303@cam.ac.uk>
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

(* 
 * Synchronization with the master
 * Client periodically communicates with the master via git
 * First merges master updates
 * Second pushes updates to the master
 *)
open Lwt
open Irmin_unix

let store = Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String)

let create local =
  let config = Irmin_git.config ~root:local ~bare:true () in
  Irmin.create store config task

let pull_exn ?depth ?(pull_type=`Merge) upstream t =
  let msg = "Synching with upstream store" in
  Irmin.pull_exn (t msg) ?depth upstream pull_type

let push_exn ?depth upstream t =
  let msg = "Pushing to upstream store" in
  Irmin.push_exn (t msg) ?depth upstream

(* how is depth controlled ??? TBD *)
let sync user mlogout config =
  let open Server_config in
  let open Irmin_storage in
  match config.master with
  | Some master 
    when config.replicate && master <> "localhost" && master <> "127.0.0.1" && config.data_store = `Irmin ->
  async(fun () ->
  let local = Utils.user_path ~path:srv_config.irmin_path ~user () in
  let path = 
    match srv_config.master_repo with
    | Some repo -> Utils.user_path ~path:repo ~user ()
    | None -> local
  in
  let remote = Printf.sprintf "git://%s%s" master path in
  let upstream = Irmin.remote_uri remote in
  Log_.log `Info3 (Printf.sprintf "### synching local %s with remote %s\n" local remote);
  let pick () =
    Lwt.pick [
      Lwt_mutex.lock mlogout >> return `Done;
      Lwt_unix.sleep config.replicate_interval >> return `Timeout; 
    ]
  in
  let rec _maintenance ?(pull_type=`Merge) () =
    catch (fun () ->
      (* need to synchronize??? with the client access or the versioning takes
       * care of this? *)
      create local >>= fun t ->
      pull_exn ~pull_type upstream t >>
      push_exn upstream t >>
      pick ()
    ) 
    (fun ex -> 
      Log_.log `Error (Printf.sprintf "### replication maintenance error: %s\n" (Printexc.to_string ex));
      pick ()
    ) >>= function
    | `Done -> return ()
    | `Timeout -> _maintenance () 
  in
  Ssl_.get_user_keys ~user Server_config.srv_config >>= fun keys ->
  IrminStorage.create Server_config.srv_config user "INBOX" keys >>= fun store ->
  IrminStorage.exists store >>= function
  | `No -> _maintenance ~pull_type:`Update ()
  | _ -> _maintenance ()
  );
  | _ -> ()
