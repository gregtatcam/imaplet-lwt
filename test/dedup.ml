open Lwt
open Imaplet
open Commands
open Parsemail

module MapStr = Map.Make(String)

let dedup = ref MapStr.empty

let () =
  Lwt_main.run (
    let config = {Server_config.srv_config with compress = false;compress_attach = false; encrypt = false } in
    Utils.fold_email_with_file1 Sys.argv.(1) (fun cnt message ->
      Printf.printf "message %d, size %d\n%!" cnt ((String.length
      (Mailbox.Message.to_string message))/(1024*1024));
      Ssl_.get_system_keys Server_config.srv_config >>= fun (pub,_) ->
      Email_parse.parse pub config message ~save_message:(fun _ _ _ _ _ -> return ())
      ~save_attachment:(fun _ _ attachment -> 
        let hash = Imap_crypto.get_hash ~hash:`Sha1 attachment in
        if MapStr.exists (fun k _ -> k = hash) !dedup then (
          let (v,s,c) = MapStr.find hash !dedup in
          dedup := MapStr.add hash (v+1,s,c) !dedup;
          Printf.printf "found duplicate %s,%d,%d\n" hash (v+1) s
        ) else (
          let comps = String.length (Imap_crypto.do_compress ~header:true attachment) in
          dedup := MapStr.add hash (1, String.length attachment,comps) !dedup
        );
        return ()
      ) >>= fun _ -> return (`Ok (cnt+1))
    ) 1 >>= fun _ ->
    let (dup,total,ctotal) = MapStr.fold (fun k (cnt,size,comps) (dup,total,ctotal) ->
      if cnt > 1 then (
        (dup+1,total+size*(cnt-1),ctotal+comps*(cnt-1))
      ) else (
        (dup,total,ctotal)
      )
    ) !dedup (0,0,0) in
    Printf.printf "duplicate attachments: %d, size %d, compressed size %d\n%!" dup total ctotal;
    return ()
  )
