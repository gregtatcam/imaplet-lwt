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
open Context
open Lwt

(* synchronization, is it needed ? *)
let connections: client_context list ref = ref []
let connid = ref Int64.zero

let next_id () =
  connid := Int64.add !connid Int64.one;
  !connid

let fold f init =
  List.fold_left (fun acc i ->
    f acc i 
  ) init !connections

let find_id id f init =
  let ctx : client_context option ref = ref None in
  if List.exists (fun (c:client_context) -> 
    if Int64.compare c.id id = 0 then ( ctx := Some c; true) else false) !connections then
    f (Utils.option_value_exn !ctx)
  else
    return init 

let update_id ?mailbox ?idle ?selected ?modseq ?capability id =
  List.exists (fun (c:client_context) ->
    if id = c.id then (
      c.idle := Utils.option_value ~default:c.!idle idle;
      c.selected := Utils.option_value ~default:c.!selected selected;
      c.modseq := Utils.option_value ~default:c.!modseq modseq; 
      c.mailbox := Utils.option_value ~default:c.!mailbox mailbox; 
      begin
      match capability with
      |Some cap -> c.capability := List.concat [cap; c.!capability]
      |None->()
      end;
      true
    ) else
      false
  ) !connections

let rem_id id =
  connections := List.fold_left (fun acc (i:client_context) ->
  if Int64.compare id i.id = 0 then
    acc
  else
    i :: acc
  ) [] !connections

let add_id id ?(idle=false) ?(selected=false) ?(modseq=Int64.zero) ?(mailbox="") user outch capability =
  if update_id ~mailbox ~idle ~selected ~modseq ~capability id = false then
    connections := {mailbox=ref mailbox;id;user;outch;capability = ref [];
     idle=ref idle;selected=ref selected;modseq=ref modseq} :: !connections
  else
    ()

let add_capability id cap =
  let _ = update_id ~capability:[cap] id in
  ()
