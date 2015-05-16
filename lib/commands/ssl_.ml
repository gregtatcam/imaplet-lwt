(*
 * Copyright (c) 2013-2014 Gregory Tsipenyuk <gt303@cam.ac.uk>
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
open Server_config
open X509.Encoding.Pem

exception PasswordRequired

type keys = Nocrypto.Rsa.pub * Nocrypto.Rsa.priv

let get_pub_key priv =
  Nocrypto.Rsa.pub_of_priv priv

let create_cert config = 
  X509_lwt.private_of_pems
  ~cert:(Install.data_path ^ "/" ^ config.pem_name)
  ~priv_key:(Install.data_path ^ "/" ^ config.key_name)

let create_user_cert ~cert ~priv_key =
  X509_lwt.private_of_pems ~cert ~priv_key

let key_encrypted key =
  let prefix = "-----BEGIN RSA PRIVATE KEY-----" in
  (String.length key > String.length prefix &&
  String.sub key 0 (String.length prefix) = prefix) = false

let get_user_keys ~user ?pswd config =
  let path name =
    let p = Filename.concat config.user_cert_path name in
    Regex.replace ~regx:"%user%" ~tmpl:user p 
  in
  Lwt_io.with_file ~mode:Lwt_io.input (path config.key_name) (fun r ->
    Lwt_io.read r
  ) >>= fun key ->
  let key =
  if key_encrypted key then (
    match pswd with
    | Some pswd ->
      let pswd = Imap_crypto.get_hash_raw (user ^ "\000" ^ pswd) in
      (Imap_crypto.aes_decrypt_pswd ~pswd key)
    | None -> raise PasswordRequired
  ) else ( (* should raise if auth_required and the key is not encrypted? TBD *)
    key 
  )
  in
  let priv = (PK.of_pem_cstruct1 (Cstruct.of_string key)) in
  return (get_pub_key priv,priv)

let get_system_keys config =
  create_cert config >>= fun (_,priv) ->
  return (get_pub_key priv,priv)

let init_ssl config =  
  Tls_lwt.rng_init () >>
  create_cert config >>= fun cert ->
  return (`Single cert)
