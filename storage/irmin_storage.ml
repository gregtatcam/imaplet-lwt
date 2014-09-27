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
open Storage
open Irmin_core
open Storage_meta

type storage = {user:string;mailbox:IrminMailbox.t}

module IrminStorage : Storage_intf with type t = string =
struct
  type t = string 

  (* user *)
  let create user =
    user

  let exists t mailbox = 
    IrminMailbox.create t mailbox >>= fun mbox ->
    IrminMailbox.exists mbox

  (* status *)
  let status t mailbox =
    IrminMailbox.create t mailbox >>= fun mbox ->
    IrminMailbox.read_mailbox_metadata mbox

  (* select mailbox *)
  let select t mailbox =
    status t mailbox

  (* examine mailbox *)
  let examine t mailbox =
    status t mailbox

  (* create mailbox *)
  let create_mailbox t mailbox  =
    IrminMailbox.create t mailbox >>= fun mbox ->
    IrminMailbox.create_mailbox mbox >>
    IrminMailbox.commit mbox

  (* delete mailbox *)
  let delete t mailbox = 
    IrminMailbox.create t mailbox >>= fun mbox ->
    IrminMailbox.delete_mailbox mbox >>
    IrminMailbox.commit mbox

  (* rename mailbox1 mailbox2 *)
  let rename t mailbox1 mailbox2 =
    IrminMailbox.create t mailbox1 >>= fun mbox ->
    IrminMailbox.move_mailbox mbox mailbox2 >>
    IrminMailbox.commit mbox

  (* subscribe mailbox *)
  let subscribe t mailbox =
    Subscriptions.create t >>= fun sub ->
    Subscriptions.subscribe sub mailbox

  (* unsubscribe mailbox *)
  let unsubscribe t mailbox =
    Subscriptions.create t >>= fun sub ->
    Subscriptions.unsubscribe sub mailbox

  (* list 
   * returns list of files/folders with list of flags 
   *)
  let list t ~subscribed ?(access=(fun _ -> true)) mailbox ~init ~f =
    IrminMailbox.create t mailbox >>= fun mbox ->
    IrminMailbox.list mbox ~subscribed ~access ~init ~f

  (* append message(s) to selected mailbox *)
  let append t mailbox message message_metadata =
    IrminMailbox.create t mailbox >>= fun mbox ->
    IrminMailbox.append_message mbox message message_metadata >>
    IrminMailbox.commit mbox

  (* expunge, permanently delete messages with \Deleted flag 
   * from selected mailbox 
   *)
  let expunge t mailbox f =
    let open Core.Std in
    let open Imaplet_types in
    IrminMailbox.create t mailbox >>= fun mbox ->
    IrminMailbox.read_index_uid mbox >>= fun uids ->
    IrminMailbox.read_mailbox_metadata mbox >>= fun mailbox_metadata ->
    Lwt_list.fold_left_s (fun (count,nunseen,recent,unseen) uid ->
      IrminMailbox.read_message_metadata mbox (`UID uid) >>= function
      | `Ok message_metadata ->
        let find_flag fl = List.find message_metadata.flags ~f:(fun f -> f = fl) <> None in
        if find_flag Flags_Deleted then (
          IrminMailbox.delete_message mbox (`UID uid) >>
          f uid >>
          return (count,nunseen,recent,unseen)
        ) else (
          let count = count + 1 in
          let recent = if find_flag Flags_Recent then recent + 1 else recent in
          let (nunseen,unseen) =
            if find_flag Flags_Seen = false then (
              if unseen = 0 then
                (nunseen + 1, count)
              else
                (nunseen + 1, unseen)
            ) else (
              (nunseen, unseen)
            )
          in
          return (count, nunseen, recent, unseen)
        )
      | _ -> return (count,nunseen,recent,unseen)
    ) (0,0,0,0) uids >>= fun (count,nunseen,recent,unseen) ->
    let modseq = Int64.(+) mailbox_metadata.modseq Int64.one in
    let mailbox_metadata = {mailbox_metadata with count;nunseen;recent;unseen;modseq} in
    IrminMailbox.update_mailbox_metadata mbox mailbox_metadata >>
    IrminMailbox.commit mbox

  (* search selected mailbox *)
  let search t mailbox keys buid =
    IrminMailbox.create t mailbox >>= fun mbox ->
    IrminMailbox.read_index_uid mbox >>= fun uids ->
    IrminMailbox.read_mailbox_metadata mbox >>= fun mailbox_metadata ->
    Lwt_list.fold_left_s (fun (seq,acc) uid ->
      IrminMailbox.read_message mbox (`UID uid)?filter:(Some keys) >>= function
      | `Ok _ -> return (if buid then (seq+1,uid :: acc) else (seq+1, seq :: acc))
      | _ -> return (seq+1,acc)
    ) (1,[]) uids >>= fun (_,acc) -> return acc

  (* fetch messages from selected mailbox *)
  let fetch t mailbox position =
    IrminMailbox.create t mailbox >>= fun mbox ->
    IrminMailbox.read_message mbox position

  (* fetch messages from selected mailbox *)
  let fetch_message_metadata t mailbox position =
    IrminMailbox.create t mailbox >>= fun mbox ->
    IrminMailbox.read_message_metadata mbox position

  (* store flags to selected mailbox *)
  let store t mailbox position message_metadata =
    IrminMailbox.create t mailbox >>= fun mbox ->
    IrminMailbox.update_message_metadata mbox position message_metadata >>= fun _ ->
    IrminMailbox.commit mbox

  (* copy messages from selected mailbox *)
  let copy t mailbox1 mailbox2 sequence buid =
    IrminMailbox.create t mailbox1 >>= fun mbox1 ->
    IrminMailbox.create t mailbox2 >>= fun mbox2 ->
    IrminMailbox.copy_mailbox mbox1 mbox2 sequence buid >>
    IrminMailbox.commit mbox2
end
