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

module IrminStorage : Storage_intf with type t = string =
struct
  type t = string 

  (* user *)
  let create user =
    user

  (* status *)
  let status t mailbox =
    let open Irmin_core in
    IrminMailbox.create "dovecot" mailbox >>= fun _ ->
    return `NotExists

  (* select mailbox *)
  let select t mailbox =
    return `NotExists

  (* examine mailbox *)
  let examine t mailbox =
    return `NotExists

  (* create mailbox *)
  let create_mailbox t mailbox  =
    return `Ok

  (* delete mailbox *)
  let delete t mailbox = 
    return `Ok

  (* rename mailbox1 mailbox2 *)
  let rename t mailbox1 mailbox2 =
    return `Ok

  (* subscribe mailbox *)
  let subscribe t mailbox =
    return `Ok

  (* unsubscribe mailbox *)
  let unsubscribe t mailbox =
    return ()

  (* list reference mailbox 
   * returns list of files/folders with list of flags 
   *)
  let list t reference mailbox =
    return []

  (* lsub reference mailbox - list of subscribed mailboxes
   * returns list of files/folders with list of flags 
   *)
  let lsub t reference mailbox =
    return []

  (* append message(s) to selected mailbox *)
  let append t mailbox ci time flags literal =
    return `Ok

  (* expunge, permanently delete messages with \Deleted flag 
   * from selected mailbox 
   *)
  let expunge t mailbox f =
    return ()

  (* search selected mailbox *)
  let search t mailbox f keys buid =
    return `Ok

  (* fetch messages from selected mailbox *)
  let fetch t mailbox f sequence fetchAttr buid =
    return `Ok

  (* store flags to selected mailbox *)
  let store t mailbox f sequence storeFlags flags buid =
    return `Ok

  (* copy messages from selected mailbox *)
  let copy t mailbox1 mailbox2 sequence buid =
    return `Ok
end
