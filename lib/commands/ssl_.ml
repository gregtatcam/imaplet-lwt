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

let create_cert config = 
  X509_lwt.private_of_pems
  ~cert:(Install.data_path ^ "/" ^ config.pem_name)
  ~priv_key:(Install.data_path ^ "/" ^ config.key_name)

let create_cert_decode ?pswd config =
  let open X509.Encoding.Pem in
  match pswd with
  | None -> create_cert config
  | Some pswd -> 
    let content f =
      Lwt_io.with_file ~mode:Lwt_io.input (Install.data_path ^ "/" ^ f) (fun ci ->
        Lwt_io.read ci
      )
    in
    content config.pem_name >>= fun pem ->
    content config.key_name >>= fun enckey ->
    let key = Imap_crypto.aes_decrypt_pswd ~pswd enckey in
    return (
      Cert.of_pem_cstruct (Cstruct.of_string pem),
      PK.of_pem_cstruct1 (Cstruct.of_string key))
  

let init_ssl config =  
  Tls_lwt.rng_init () >>
  create_cert config >>= fun cert ->
  return (`Single cert)
