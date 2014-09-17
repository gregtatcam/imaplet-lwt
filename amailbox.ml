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
open StorageMeta
open Imaplet_types

type selection = [`Select of string | `Examine of string | `None]

(* inbox location * all mailboxes location * user * type of selected mailbox *)
type t = string * string * string option * selection 

let selected_mbox mbx =
  let (_,_,_,s) = mbx in
  match s with
  | `None -> None
  | `Select s -> Some s
  | `Examine s -> Some s

(* create the mailbox type *)
let create user =
  (* choose mailbox type from configuration, i.e. irmin etc *)
  ("","",Some user,`None)

(* empty type *)
let empty () =
  ("","",None,`None)

(* get authenticated user *)
let user mailboxt =
  let (_,_,u,_) = mailboxt in
  u

(* list mailbox *)
let listmbx mailboxt reference mailbox =
  return []

(* list subscribed *)
let lsubmbx mailboxt reference mailbox =
  return []

(* select mailbox : `NotExists, `NotSelectable, `Error, `Ok *)
let select mailboxt mailbox = 
  return (`Ok (mailboxt, empty_mailbox_metadata ()))

(* examine mailbox *)
let examine mailboxt mailbox =
  return (`Ok (mailboxt, empty_mailbox_metadata ()))

(* create mailbox *)
let create_mailbox mailboxt mailbox =
  return `Ok

(* delete mailbox *)
let delete_mailbox mailboxt mailbox =
  return `Ok

(* rename mailbox *)
let rename_mailbox mailboxt src dest =
  return `Ok

(* subscribe mailbox *)
let subscribe mailboxt mailbox =
  return `Ok

(* unsubscribe mailbox *)
let unsubscribe mailboxt mailbox =
  return `Ok

(* append message to mailbox *)
let append mailboxt mailboxt reader writer flags date literal =
  return `Ok

(* close selected mailbox *)
let close mailboxt =
  let (i,m,u,_) = mailboxt in
  (i,m,u,`None)

(* search mailbox *)
let search mailboxt keys buid =
  return (`Ok [])

(* fetch data from mailbox *)
let fetch mailboxt resp_writer sequence fetchattr buid =
  return `Ok

(* store flags *)
let store mailboxt resp_writer sequence storeattr flagsval buid =
  return `Ok

(* copy messages to mailbox *)
let copy mailboxt dest_mbox sequence buid =
  return `Ok

(* permanently remove messages with \Deleted flag *)
let expunge mailboxt resp_writer =
  return `Ok
