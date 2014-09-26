open Lwt
open Core.Std
open Irmin_core
open BatLog

Easy.level := `always;
Easy.output := BatIO.stderr;;

let () = Lwt_main.run(
  let mailboxes = [
    "/inbox";
    "/testF1/";
    "/testF1/testM11";
    "/testF1/testM12";
    "/testF1/testM13";
    "/testF1/testF11/";
    "/testF1/testF11/testM131";
    "/testF1/testF12/";
    "/testF1/testF12/testM231";
    "/testF1/testF12/testM232";
    "/testF2/";
    "/testF2/testM21";
    "/testF2/testF21/";
    "/testF2/testF22/";
    "/testF2/testF23/";
    "/testF2/testF23/testM231";
    "/testF2/testF24/";
    "/testF2/testF24/testM234";] in
  IrminIntf.create () >>= fun store ->
  IrminIntf.remove store (Key_.create_account "dovecot") >>= fun () ->
  let rec create = function
    | hd :: tl -> 
      Printf.printf "creating %s\n%!" hd;
      IrminMailbox.create "dovecot" hd >>= fun mbox ->
      IrminMailbox.create_mailbox mbox >>
      IrminMailbox.commit mbox >>= fun () ->
      create tl
    | [] -> return ()
  in
  create mailboxes >>= fun () ->
  IrminMailbox.create "dovecot" "" >>= fun mbox ->
    IrminMailbox.list ~subscribed:false ~access:(fun _ -> true) mbox ~init:[] ~f:(
      fun acc item -> return ((item::acc))
    ) >>= fun listing ->
  List.iter listing ~f:(fun i -> match i with
    | `Folder (f,cnt) -> Printf.printf "folder: %s %d\n%!" f cnt
    | `Mailbox m -> Printf.printf "mailbox: %s\n%!" m
  ); return ()
)
