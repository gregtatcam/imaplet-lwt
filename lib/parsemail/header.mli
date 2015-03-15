open Core_replace

type t = string Field_name.Assoc.t;;

(** General raw accessors *)

(** Raw access is NOT recommended. Use the Typed modules instead *)
include Field_name.Assoc_concrete (String).S with type t := t;;
include String_monoidable.S with type t := t;;

(*****************************)
(* Typed header-field access *)
(*****************************)


(** This interfaces provide type-safe access to the contents of the header. *)
module Typed_intf : sig
  module type S = sig
    type el
    (** Equivalent to Option.try_with (fun () -> last_exn [args]) *)
    val last : t -> ?on_error:[`Ignore | `Stop ] -> Field_name.t -> el option

    (** Parses the last field with the given name. On_error determines
      what to do if the parsing returns an error:
        `Ignore: Try to find another element.
        `Stop: Raise the exception returned by the parsing.
      If no field with name is found, raises Not_found.
    *)
    val last_exn : t -> ?on_error:[`Ignore | `Stop ] -> Field_name.t -> el

    (** All parsable fields *)
    val all : t -> (Field_name.t * el) list

    (** All fields, raises exception if unable to parse *)
    val all_exn : t -> (Field_name.t * el) list

    (** All parsable fields with name *)
    val all_name : t -> Field_name.t -> el list

    (** All field with name, raises exception if unable to parse *)
    val all_name_exn : t -> Field_name.t -> el list

    val add : t -> name:Field_name.t -> el -> t
    val set : t -> name:Field_name.t -> el -> t
  end
end

(** Same as Typed, but tailored to access a field with a specific name *)
module Typed_single_intf : sig
  module type S = sig
    type el
    (** Last instance of field, raises error if unable to parse *)
    val last_exn : t -> el

    (** Last parsable instance of field *)
    val last : t -> el option

    (** All fields with name *)
    val all : t -> el list

    (** All fields with name, raises error if unable to parse one of them. *)
    val all_exn : t -> el list

    val add : t -> el -> t

    (** Replace last instance of the field, create new if existing. *)
    val set : t -> el -> t

    val remove_last : t -> t
    val remove_all  : t -> t
  end
end


module type Constant = sig
  type t
  val t : t
end

module Typed (T : Stringable_.S) : Typed_intf.S with type el := T.t
module Typed_single (T : Stringable_.S) (C : Constant with type t := Field_name.t)
  : Typed_single_intf.S with type el := T.t

(** Accesses any field's contents as a string *)
module Unstructured : Typed_intf.S with type el := string

(** Accesses "Content-type" fields *)
module Content_type : sig
  include Typed_single_intf.S with type el := Media_type.t
  val default : parent:(Media_type.t option) -> Media_type.t
end

(** Accesses "Content-transfer-encoding" fields *)
module Content_transfer_encoding : sig
  include Typed_single_intf.S with type el := Encoding.t
  val default : Encoding.t
end


