(*
 * Copyright (c) 2013-2015 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
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
open Nocrypto

let pad c_data padding =
  let len = Cstruct.len c_data in
  let m = Pervasives.(mod) len 32 in
  if m > 0 then
    (32-m,Cstruct.of_string (Bytes.cat (Cstruct.to_string c_data) (Bytes.sub (Cstruct.to_string padding) 0 (32-m))))
  else
    (0,c_data)

let refill input =
  let n = String.length input in
  let toread = ref n in
  fun buf ->
    let m = min !toread (String.length buf) in
    String.blit input (n - !toread) buf 0 m;
    toread := !toread - m;
    m

let flush output buf len =
  Buffer.add_substring output buf 0 len

let do_compress input =
  let output = Buffer.create (String.length input) in
  Zlib.compress ~level:6 (refill input) (flush output);
  Buffer.contents output

let do_uncompress input =
  let output = Buffer.create (String.length input) in
  Zlib.uncompress (refill input) (flush output);
  Buffer.contents output

let aes_encrypt ?(compress=false) data pub secrets =
  let open Nocrypto.Cipher_block in
  let data = if compress then do_compress data else data in
  let (secret,iv) = secrets data in
  let key = AES.CBC.of_secret secret in
  let (pad_size,c_data) = pad (Cstruct.of_string data) secret in
  let encr = AES.CBC.encrypt ~key ~iv c_data in
  let encrypted = Cstruct.to_string encr.message in
  let header = Printf.sprintf "%02d%s%s" pad_size (Cstruct.to_string secret) (Cstruct.to_string iv) in
  let header1 = Cstruct.to_string (Rsa.encrypt ~key:pub (Cstruct.of_string header)) in
  (secret,iv,Printf.sprintf "%04d%s%s" (Bytes.length header1) header1 encrypted)

let aes_decrypt ?(compressed=false) data priv =
  let open Nocrypto.Cipher_block in
  Rng.reseed (Cstruct.of_string "abc");
  let header_size = int_of_string (Bytes.sub data 0 4) in
  let header = Bytes.sub data 4 header_size in
  let encrypted = Bytes.sub data (4 + header_size) ((Bytes.length data) - 4 - header_size) in
  let header_decr = Rsa.decrypt ~key:priv (Cstruct.of_string header) in
  let header1 = Bytes.sub (Cstruct.to_string header_decr) ((Cstruct.len header_decr) - 66) 66 in
  let pad_size = int_of_string (Bytes.sub header1 0 2 ) in
  let hash = Cstruct.of_string (Bytes.sub header1 2 32) in
  let iv = Cstruct.of_string (Bytes.sub header1 34 32) in
  let key = AES.CBC.of_secret hash in
  let decr = AES.CBC.decrypt ~key ~iv (Cstruct.of_string encrypted) in
  let decrypted = Cstruct.to_string decr.message in
  let data = Bytes.sub decrypted 0 ((Bytes.length decrypted) - pad_size) in
  if compressed then do_uncompress data else data

(* have to use different IV every time TBD!!!*)
let encrypt ?(compress=false) data pub =
  let (_,_,e) = 
    aes_encrypt ~compress data pub (fun _ -> 
      Rng.reseed (Cstruct.of_string "abc"); (Rng.generate 32, Rng.generate 32)) in
  e

let decrypt ?(compressed=false) data priv =
  aes_decrypt ~compressed data priv

let get_hash data =
  let contid = Cstruct.to_string (Base64.encode (Hash.SHA1.digest (Cstruct.of_string data))) in
  Str.global_replace (Str.regexp "/") "o057" contid

let contentid hash =
  let contid = Cstruct.to_string (Base64.encode (Hash.SHA1.digest hash)) in
  Str.global_replace (Str.regexp "/") "o057" contid

let conv_encrypt ?(compress=false) data pub =
  let (hash,_,e) = aes_encrypt ~compress data pub (fun data -> 
    let hash = Hash.SHA256.digest (Cstruct.of_string data) in (hash,hash)
  ) in
  (contentid hash,e)

let conv_decrypt ?(compressed=false) data priv =
  aes_decrypt ~compressed data priv
