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
open Sexplib.Std
open Imaplet_types

type mailbox_metadata = {
  uidvalidity: string;
  uidnext: int;
  modseq: int64;
  count: int;
  unseen: int;
  nunseen: int;
  recent: int;
  selectable: bool;
} [@@deriving sexp]

type mbox_mailbox_metadata = {
  mbox_metadata: mailbox_metadata;
  uids: int list;
}

type mailbox_message_metadata = {
  uid: int;
  modseq: int64;
  internal_date: Dates.ImapTime.t;
  size: int;
  flags: mailboxFlags list;
} [@@deriving sexp]

type mbox_message_metadata = {
  metadata: mailbox_message_metadata;
  start_offset: int64;
  end_offset: int64;
}

type mbox_index = [`Header of mbox_mailbox_metadata | `Record of mbox_message_metadata]

let new_uidvalidity() =
  let t = Dates.ImapTime.now() in
  let f = (Dates.ImapTime.to_float t) *. 100. in
  Int64.to_string (Int64.of_float f)

let empty_mailbox_metadata ?uidvalidity ?(selectable=true) () =
  let uidvalidity =
    (match uidvalidity with None->""|Some u -> u) in
  {uidvalidity;modseq=Int64.zero;uidnext=1;count=0;unseen=0;nunseen=0;recent=0;selectable}

let empty_mailbox_message_metadata() =
  {uid=1;modseq=Int64.zero;internal_date=Dates.ImapTime.now();size=0;flags=[]}

let empty_mbox_message_metadata() =
  {metadata=empty_mailbox_message_metadata();start_offset=Int64.zero;end_offset=Int64.zero}

let update_mailbox_metadata ~header ?uidvalidity ?uidnext ?modseq
?count ?unseen ?nunseen ?recent ?selectable () : (mailbox_metadata) =
  let uidvalidity=(match uidvalidity with None->header.uidvalidity|Some u->u) in
  let uidnext=(match uidnext with None->header.uidnext|Some u->u) in
  let modseq=(match modseq with None->header.modseq|Some m->m) in
  let count=(match count with None->header.count|Some c->c) in
  let unseen=(match unseen with None->header.unseen|Some u->u) in
  let nunseen=(match nunseen with None->header.nunseen|Some n->n) in
  let recent=(match recent with None->header.recent|Some r->r) in
  let selectable=(match selectable with None->header.selectable|Some r->r) in
  {uidvalidity;modseq;uidnext;count;unseen;nunseen;recent;selectable}

let update_mailbox_message_metadata ~data ?uid ?modseq
?internal_date ?size ?flags () : (mailbox_message_metadata) =
  let uid =
    (match uid with |None -> data.uid|Some uid -> uid) in
  let modseq =
    (match modseq with |None -> data.modseq|Some modseq -> modseq) in
  let internal_date =
    (match internal_date with |None->data.internal_date|Some d -> d) in
  let size =
    (match size with |None->data.size|Some i -> i) in
  let flags = 
    (match flags with |None->data.flags|Some f->f) in
  {uid;modseq;internal_date;size;flags}

let update_mbox_message_metadata ~data ?uid ?modseq
?internal_date ?size ?start_offset ?end_offset ?flags () : (mbox_message_metadata) =
  let metadata = update_mailbox_message_metadata ~data:data.metadata ?uid ?modseq ?internal_date
  ?size ?flags () in
  let start_offset =
    (match start_offset with |None->data.start_offset|Some i -> i) in
  let end_offset =
    (match end_offset with |None->data.end_offset|Some i -> i) in
  {metadata;start_offset;end_offset}
