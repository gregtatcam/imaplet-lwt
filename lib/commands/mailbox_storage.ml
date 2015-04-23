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

module MailboxStorage : Storage_intf with type t = string =
struct
  type t = string

  (* user *)
  let create config user mailbox keys =
    return user

  let exists t = 
    return `No

  (* status *)
  let status t =
    return (empty_mailbox_metadata())

  (* select mailbox *)
  let select t =
    status t 

  (* examine mailbox *)
  let examine t =
    status t 

  (* create mailbox *)
  let create_mailbox t =
    return ()

  (* delete mailbox *)
  let delete t = 
    return ()

  (* rename mailbox1 mailbox2 *)
  let rename t mailbox2 =
    return ()

  (* subscribe mailbox *)
  let subscribe t =
    return ()

  (* unsubscribe mailbox *)
  let unsubscribe t =
    return ()

  (* list 
   * returns list of files/folders with list of flags 
   *)
  let list t ~subscribed ?(access=(fun _ -> true)) ~init ~f =
    return init

  (* append message(s) to selected mailbox *)
  let append t message message_metadata =
    return ()

  (* delete a message *)
  let delete_message t position =
    return ()

  (* search selected mailbox *)
  let search t keys buid =
    return []

  (* fetch messages from selected mailbox *)
  let fetch t position =
    return `NotFound

  (* fetch messages from selected mailbox *)
  let fetch_message_metadata t position =
    return `NotFound

  (* store flags to selected mailbox *)
  let store t position message_metadata =
    return ()

  (* store mailbox metadata *)
  let store_mailbox_metadata t mailbox_metadata =
    return ()

  (* copy messages from selected mailbox *)
  let copy t pos t2 message_metadata =
    return ()

  let commit t =
    return ()

  let uid_to_seq t uid =
    return None
end
