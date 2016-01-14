open Lwt
open Sexplib
open Imaplet
open Commands
open Lightparsemail

let pr message ent msg = 
  let len = ent.size in
  let offset = ent.offset in
  let prfx = if len > 50 then String.sub message offset 50 else
    String.sub message offset len in
  let pstfx = if len > 100 then String.sub message (offset+len - 50) 50 else "" in
  Printf.printf "  %s: %d %s...%s\n%!" msg ent.size prfx pstfx

let rec walk message (email:lightmail) =
  pr message email.headers.position "Headers";
  match email.body.content with
  | `Data -> pr message email.body.position "Data"
  | `Message m -> pr message email.body.position "Message"; walk message m
  | `Multipart l -> pr message email.body.position "Multipart";
    List.iteri (fun cnt (email:lightmail) ->
      pr message email.position (Printf.sprintf "Part %d" cnt);
      walk message email
    ) l 

let () =
  Lwt_main.run (
    Utils.fold_email_with_file Sys.argv.(1) (fun cnt message ->
      Printf.fprintf stderr "%d\r%!" cnt;
      Message.parse message >>= fun light ->
      Printf.printf "--> start\n%!";
      Printf.printf "Full message %d\n%!" light.message_.position.size;
      pr message light.message_.postmark "Postmark";
      pr message light.message_.email.position "Email";
      walk message light.message_.email;
      Printf.printf "<-- end\n%!";
      return (`Ok (cnt+1))
    ) 1 >>= fun _ ->
    return ()
  )
