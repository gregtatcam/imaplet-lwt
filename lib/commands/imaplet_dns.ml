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
open Dns
open Packet

type configt = [`File of string|`NS of ((string*string) list) * (string list)]

let cache_config = ref None

let get_config config =
  match !cache_config with
  | Some config -> Some config
  | None ->
    match config with
    | None -> None
    | Some config ->
    let config =
    match config with 
    | `File file -> Some (`Resolv_conf file)
    | `NS (ips,domains) ->
      let ips = List.map (fun (ip,port) ->
        (Utils.option_value_exn (Ipaddr.of_string ip)),(int_of_string port)
      ) ips in
      Some (`Static (ips,domains))
    in
    cache_config := config;
    config

let get_resolver config =
  match (get_config config) with
  | None -> Dns_resolver_unix.create ()
  | Some config -> Dns_resolver_unix.create ~config ()

let _gethostbyname resolver domain =
  let domain =
  match domain with
  | `Name name -> name
  | `Domain domain -> Name.to_string domain 
  in
  Utils.with_timeout 10. (fun () -> Dns_resolver_unix.gethostbyname resolver domain)
  (fun _ -> return []) >>= fun ip_list ->
  return (List.fold_left (fun acc ip ->
    (Ipaddr.to_string ip) :: acc
  ) [] ip_list)

let gethostbyname ?config name =
  get_resolver config >>= fun resolver ->
  _gethostbyname resolver (`Name name)

let gethostbyaddr ?config addr =
  get_resolver config >>= fun resolver ->
  Dns_resolver_unix.gethostbyaddr resolver (Ipaddr.V4.of_string_exn addr) 

let resolve ?config domain =
  let domain = Name.string_to_domain_name domain in
  get_resolver config >>= fun resolver ->
  Utils.with_timeout 30. (fun () -> 
    Dns_resolver_unix.resolve resolver Q_IN Q_MX domain >>= fun result ->
    return (Some result)
  ) (fun _ -> return None) >>= function
  | Some result ->
  Lwt_list.fold_left_s (fun acc rr ->
    match rr.rdata with
    | MX (pri,domain) ->
      _gethostbyname resolver (`Domain domain) >>= fun ip_list ->
      return ((pri,ip_list) :: acc)
    | _ -> return acc
  ) [] result.answers >>= fun res ->
  return (List.sort (fun (pri1,_) (pri2,_) -> compare pri1 pri2) res)
  | None -> return []
