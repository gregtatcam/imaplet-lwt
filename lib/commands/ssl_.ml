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
open X509
open X509.Encoding.Pem
open Sexplib.Conv

exception PasswordRequired
exception FailedPubKey

type keys = Nocrypto.Rsa.pub * Nocrypto.Rsa.priv option

let get_pub_key priv =
  Nocrypto.Rsa.pub_of_priv priv

(* get public key from certificate string *)
let pub_of_cert_string cert =
  let cert = Certificate.of_pem_cstruct1 (Cstruct.of_string cert) in
  match (X509.public_key cert) with
  | `RSA pub_key -> pub_key
  | _ -> raise FailedPubKey

let sexp_of_pub pub =
  Nocrypto.Rsa.sexp_of_pub pub

let pub_of_sexp sexp =
  Nocrypto.Rsa.pub_of_sexp sexp

(* create certificate configuration from certificate and 
 * private key server files *)
let create_cert config = 
  X509_lwt.private_of_pems
  ~cert:(Install.data_path ^ "/" ^ config.pem_name)
  ~priv_key:(Install.data_path ^ "/" ^ config.key_name)

(* check if the private key is encrypted *)
let key_encrypted key =
  let prefix = "-----BEGIN RSA PRIVATE KEY-----" in
  (String.length key > String.length prefix &&
  String.sub key 0 (String.length prefix) = prefix) = false

(* get user public and private keys 
 * decrypt the private key file if encrypted
 * get the public key from the certificate file
 *)
let get_user_keys ~user ?pswd config =
  let _path name =
    let path = Filename.concat config.user_cert_path name in
    Utils.user_path ~user ~path () 
  in
  let of_pem key =
    match (Private_key.of_pem_cstruct1 (Cstruct.of_string key)) with
    | `RSA priv_key -> priv_key
  in
  Lwt_io.with_file ~mode:Lwt_io.input (_path config.key_name) (fun r ->
    Lwt_io.read r
  ) >>= fun key ->
  let priv =
  if key_encrypted key then (
    match pswd with
    | Some pswd ->
      let pswd = Imap_crypto.get_hash_raw (user ^ "\000" ^ pswd) in
      Some (of_pem (Imap_crypto.aes_decrypt_pswd ~pswd key))
    | None -> None (*raise PasswordRequired*)
  ) else ( (* should raise if auth_required and the key is not encrypted? TBD *)
    Some (of_pem key)
  )
  in
  Lwt_io.with_file ~mode:Lwt_io.input (_path config.pem_name) (fun r ->
    Lwt_io.read r
  ) >>= fun cert ->
  return (pub_of_cert_string cert, priv)

let get_system_keys config =
  create_cert config >>= fun (_,priv) ->
  return (get_pub_key priv,priv)

(* initialize ssl, create certificate *)
let init_ssl config =  
  Nocrypto_entropy_lwt.initialize () >>
  create_cert config >>= fun cert ->
  return (`Single cert)
