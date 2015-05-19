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
open Socket_utils

exception InvalidField of int * int * int
exception InvalidResponse of string
exception Timeout

let transid () =
  let open Nocrypto.Rng in
  let open Cstruct in
  reseed (of_string (string_of_float (Unix.gettimeofday())));
  let id = ref [] in
  String.iter (fun c -> id := (int_of_char c) :: !id) (to_string (generate 12));
  List.rev !id

let msg_type = [0x00;0x01] (* class 0, method 1, method assignment 0 *)
let msg_length = [0x00;0x08]
let magic_cookie = [0x21;0x12;0xa4;0x42] (* magic cookie - fixed value *)
let msg_transid = [0xa6;0x43;0x63;0xc6;0x88;0xcd;0xfd;0xb3;0xff;0x7f;0x00;0x00]
let msg_attr_type = [0x00;0x03] (* type CHANGE_REQUEST 3, comprehension 0, assignment 0 *)
let msg_attr_length = [0x00;0x04]
let msg_attr = [0x00;0x00;0x00;0x00] (* change ip 0(not set), change port 0(not set) *)

let request () =
  let msg_transid = transid () in
  let msg = List.concat
    [msg_type;msg_length;magic_cookie;msg_transid;msg_attr_type;msg_attr_length;msg_attr]
  in
  let b = List.fold_left (fun buff i -> 
    Buffer.add_char buff (char_of_int i); buff) (Buffer.create (List.length msg)) msg
  in
  (msg_transid,Buffer.contents b)

let field resp offset size =
  if (offset + size) > (String.length resp) then
    raise (InvalidField (String.length resp,offset,size));
  let rec iter i acc =
    if (i-offset) = size then
      acc
    else (
      let acc = (int_of_char resp.[i]) :: acc in
      iter (i+1) acc
    )
  in
  List.rev (iter offset [])

let get_length length =
  let length = (lor) ((lsl) (List.hd length) 8) (List.nth length 1) in
  let m = (mod) length 4 in
  (* values are padded to 4 bytes, length contains the value part prior to
   * padding *)
  if m = 0 then
    length
  else
    length + 4 - m

let validate field value ex =
  if field <> value then
    raise (InvalidResponse ex)

let xor x1 x2 =
  List.map2 (fun x1 x2 -> (lxor) x1 x2) x1 x2

let get_port ~_xor port =
  let port = if _xor then xor port [0x21;0x12] else port in
  (lor) ((lsl) (List.hd port) 8) (List.nth port 1)

let get_ip ~_xor ip =
  let ip = if _xor then xor ip magic_cookie else ip in
  List.fold_left (fun acc p ->
    if acc = "" then
      (string_of_int p)
    else
      acc ^ "." ^ (string_of_int p)
  ) "" ip

let mapped_addr = [0x00;0x001]
let xor_mapped_addr = [0x00;0x20]

let get_mapped_addr ?(_xor=false) resp offset =
  let attr_length = field resp (offset + 2) 2 in
  validate attr_length [0x00;0x08] "attribute length";
  let reserved = field resp (offset + 4) 1 in validate reserved [0x00] "marker";
  let prot_family = field resp (offset + 5) 1 in validate prot_family [0x01] "not IPv4";
  let port = field resp (offset + 6) 2 in
  let ip = field resp (offset + 8) 4 in
  (get_ip ~_xor ip,get_port ~_xor port)

let get_attributes resp =
  let rec get_attr resp offset =
    if offset + 4 > (String.length resp) then (
      None
    ) else (
      let attr_type = field resp offset 2 in
      if attr_type = xor_mapped_addr then (
        Some (get_mapped_addr ~_xor:true resp offset)
      ) else if attr_type = mapped_addr then (
        Some (get_mapped_addr resp offset)
      ) else (
        let attr_length = get_length (field resp (offset + 2) 2) in
        get_attr resp (offset + 4 + attr_length) 
      )
    )
  in get_attr resp 20 

let stun_request ?interface addr port =
  client_send_dgram ?interface (`Inet (addr,port)) (fun _  rdr wr ->
    let (transid,msg) = request() in
    wr msg >>= fun s ->
    let buff = String.create 2048 in
    Lwt.pick [
      (Lwt_unix.sleep 5. >> return `Timeout);
      (rdr buff >>= fun (size,_,_) -> return (`Ok size));] >>= function
    | `Timeout -> raise Timeout
    | `Ok size ->
    let resp = (String.sub buff 0 size) in
    let rsp_type = field resp 0 2 in validate rsp_type [0x01;0x01] "failed type";
    let rsp_length =  field resp 2 2 in 
    let rsp_cookie = field resp 4 4 in validate rsp_cookie magic_cookie "invalid cookie";
    let rsp_transid = field resp 8 12 in validate rsp_transid transid "invalid transid";
    return (get_attributes resp)
  ) None
