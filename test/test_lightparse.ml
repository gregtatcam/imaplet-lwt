open Lwt
open Sexplib
open Imaplet
open Commands
open Lightparsemail

let pr ent msg = 
  let message = ent.message in
  let len = ent.size in
  let offset = ent.offset in
  let prfx = if len > 50 then String.sub message offset 50 else
    String.sub message offset len in
  let pstfx = if len > 100 then String.sub message (offset+len - 50) 50 else "" in
  Printf.printf "  %s: %d %s...%s\n%!" msg ent.size prfx pstfx

let rec walk (email:lightmail) =
  pr email.headers.position "Headers";
  match email.body.content with
  | `Data -> pr email.body.position "Data"
  | `Message m -> pr email.body.position "Message"; walk m
  | `Multipart l -> pr email.body.position "Multipart";
    List.iteri (fun cnt (email:lightmail) ->
      pr email.position (Printf.sprintf "Part %d" cnt);
      walk email
    ) l 

let () =
  Lwt_main.run (
    Utils.fold_email_with_file Sys.argv.(1) (fun cnt message ->
      Printf.fprintf stderr "%d\r%!" cnt;
      Lightparsemail.Message.parse message >>= fun (light:lightmessage) ->
      Printf.printf "--> start\n%!";
      Printf.printf "Full message %d\n%!" light.position.size;
      pr light.postmark "Postmark";
      pr light.email.position "Email";
      walk light.email;
      Printf.printf "<-- end\n%!";
      return (`Ok (cnt+1))
    ) 1 >>= fun _ ->
    return ()
  )
