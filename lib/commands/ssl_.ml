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

type keys = Nocrypto.Rsa.pub * Nocrypto.Rsa.priv

let get_pub_key priv =
  Nocrypto.Rsa.pub_of_priv priv

let create_cert config = 
  X509_lwt.private_of_pems
  ~cert:(Install.data_path ^ "/" ^ config.pem_name)
  ~priv_key:(Install.data_path ^ "/" ^ config.key_name)

let create_user_cert ~cert ~priv_key =
  X509_lwt.private_of_pems ~cert ~priv_key

let get_user_keys ~user config =
  let path name =
    let p = Filename.concat config.user_cert_path name in
    Regex.replace ~regx:"%user%" ~tmpl:user p 
  in
  create_user_cert ~cert:(path config.pem_name) 
    ~priv_key:(path config.key_name) >>= fun (_,priv) ->
  return (get_pub_key priv,priv)

let get_system_keys config =
  create_cert config >>= fun (_,priv) ->
  return (get_pub_key priv,priv)

let init_ssl config =  
  Tls_lwt.rng_init () >>
  create_cert config >>= fun cert ->
  return (`Single cert)
