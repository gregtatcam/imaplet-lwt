open Core_replace

module Assoc_maker = Field_name.Assoc_concrete (String);;
module Assoc = Assoc_maker.Make
include Assoc;;

let to_string_monoid t =
  let field_to_string_monoid (name, body) =
    String_monoid.concat_string [name; ":"; body; "\n"]
  in
  String_monoid.concat
    (map_to_list t ~f:field_to_string_monoid)
;;

module type Constant = sig
  type t
  val t : t
end

module Typed_intf = struct
  module type S = sig
    type el
    val last : t -> ?on_error:[`Ignore | `Stop ] -> Field_name.t -> el option
    val last_exn : t -> ?on_error:[`Ignore | `Stop ] -> Field_name.t -> el
    val all : t -> (Field_name.t * el) list
    val all_exn : t -> (Field_name.t * el) list
    val all_name : t -> Field_name.t -> el list
    val all_name_exn : t -> Field_name.t -> el list
    val add : t -> name:Field_name.t -> el -> t
    val set : t -> name:Field_name.t -> el -> t
  end
end

(** Generates accessors for a typed header field, regardless of name *)
module Typed (T_unsafe : Stringable_.S) :
  Typed_intf.S with type el := T_unsafe.t
= struct

  (* Ensures that the headers are properly folded and unfolded. *)
  let to_field_string = Fn_.compose Rfc.RFC2822.fold T_unsafe.to_string
  let of_field_string = Fn_.compose T_unsafe.of_string Rfc.RFC2822.unfold

  let last_exn t ?(on_error=`Ignore) name =
    match (List.find_map (to_rev_list t)
      ~f:(fun (name', str) ->
        if Field_name.equal name name' then
          match on_error with
          | `Ignore -> Option_.try_with (fun () -> of_field_string str)
          | `Stop -> Some (of_field_string str)
        else
          None))
    with
    | Some el -> el
    | None    -> raise Not_found
  ;;

  let last t ?(on_error=`Ignore) name =
    Option_.try_with (fun () -> last_exn t ~on_error name);;

  let all t =
    List.rev_filter_map (to_rev_list t)
      ~f:(fun (field_name, str) ->
        Option_.try_with (fun () -> (field_name, of_field_string str)))
  ;;

  let all_exn t =
    List.rev_filter_map (to_rev_list t)
      ~f:(fun (field_name, str) -> Some (field_name, of_field_string str))
  ;;

  let all_name t name =
    List.rev_filter_map (to_rev_list t)
      ~f:(fun (name', str) ->
        if Field_name.equal name name' then
          Option_.try_with (fun () -> of_field_string str)
        else
          None)
  ;;

  let all_name_exn t name =
    List.rev_filter_map (to_rev_list t)
      ~f:(fun (name', str) ->
        if Field_name.equal name name' then
          Some (of_field_string str)
        else
          None)
  ;;

  let add t ~name el =
    add t ~name ~body:(to_field_string el)
  ;;

  let set t ~name el =
    set t ~name ~body:(to_field_string el)
  ;;
end
;;

(** Generates accesors for a field with a single name *)
module Typed_single_intf = struct
  module type S = sig
    type el
    val last_exn : t -> el
    val last : t -> el option
    val all : t -> el list
    val all_exn : t -> el list
    val add : t -> el -> t
    val set : t -> el -> t
    val remove_last : t -> t
    val remove_all  : t -> t
  end
end

module Typed_single (T : Stringable_.S) (C : Constant with type t := Field_name.t)
  : Typed_single_intf.S with type el := T.t
= struct
  module General = Typed (T)
  let last t = General.last t C.t;;
  let last_exn t = General.last_exn t C.t;;
  let all t = General.all_name t C.t;;
  let all_exn t = General.all_name_exn t C.t;;
  let add t el = General.add t ~name:C.t el;;
  let set t el = General.set t ~name:C.t el;;
  let remove_last t = remove_last t C.t;;
  let remove_all t = remove_all t C.t;;
end

module Content_type = struct
  module Name = struct let t = "content-type" end
  include Typed_single (Media_type) (Name)

  open Media_type

  let default_default =
    {
      mime_type = "text";
      mime_subtype = "plain";
      params = Field_name.Assoc.of_rev_list [("charset","us-ascii")]
    }
  ;;

  let default_digest =
    {
      mime_type = "message";
      mime_subtype = "rfc2822";
      params = Field_name.Assoc.empty ()
    }
  ;;

  let default ~parent =
    if Option_.value_map parent ~f:Media_type.is_digest ~default:false then
      default_digest
    else
      default_default
  ;;
end


module Content_transfer_encoding = struct
  module Name = struct let t = "content-transfer-encoding" end
  include Typed_single (Encoding) (Name)

  let default = `Bit7;;
end

(** By using the Unstructured accessor, the fields are stripped of meaningless
  WSP and EOL characters *)
module Unstructured = Typed (String);;


