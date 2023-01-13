(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2019       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** {6 Interactive visit of a vo} *)



module Repr =
struct
  open Analyze

  type obj = data

  let memory = ref LargeArray.empty
  (** size, in words *)

  let input ch =
    let obj, mem = parse_channel ch in
    let () = memory := mem in
    obj

  let pr_imm len = function
  | Int n -> Printf.sprintf "i%i" n
  | Ptr p -> Printf.sprintf "0x%0*x" len p
  | Atm n -> Printf.sprintf "a%i" n
  | Fun _ -> Printf.sprintf "f"

  let pr_struct shf v = match v with
  | [| |] -> ""
  | [|v|] -> pr_imm shf v
  | _ ->
    let len = Array.length v in
    let ans = ref [] in
    for i = 0 to len - 1 do
      ans := pr_imm shf v.(i) :: !ans
    done;
    let ans = String.concat " " (List.rev !ans) in
    ans

  let dump () =
    let len = LargeArray.length !memory in
    let shf =
      if len <= 0xFF then 2
      else if len <= 0xFFFF then 4
      else if len <= 0xFFFFFFFF then 8
      else 16
    in
    for i = 0 to len - 1 do
      match LargeArray.get !memory i with
      | Struct (tag, obj) ->
        Printf.printf "0x%0*x::B:%i[%i]{%s}\n%!" shf i tag (Array.length obj) (pr_struct shf obj)
      | Int64 n ->
        Printf.printf "0x%0*x::I:%s\n%!" shf i (Int64.to_string n)
      | Float64 f ->
        Printf.printf "0x%0*x::F:%s\n%!" shf i (string_of_float f)
      | String s ->
        Printf.printf "0x%0*x::S:\"%s\"\n%!" shf i (String.escaped s)
    done

end

(** Loading the vo *)

type header = {
  magic : string;
  (** Magic number of the marshaller *)
  length : int;
  (** Size on disk in bytes *)
  size32 : int;
  (** Size in words when loaded on 32-bit systems *)
  size64 : int;
  (** Size in words when loaded on 64-bit systems *)
  objects : int;
  (** Number of blocks defined in the marshalled structure *)
}

let dummy_header = {
  magic = "\000\000\000\000";
  length = 0;
  size32 = 0;
  size64 = 0;
  objects = 0;
}

let parse_header chan =
  let magic = really_input_string chan 4 in
  let length = input_binary_int chan in
  let objects = input_binary_int chan in
  let size32 = input_binary_int chan in
  let size64 = input_binary_int chan in
  { magic; length; size32; size64; objects }

type segment = {
  name : string;
  pos : int64;
  len : int64;
  hash : Digest.t;
}

let input_int32 ch =
  let accu = ref 0l in
  for _i = 0 to 3 do
    let c = input_byte ch in
    accu := Int32.add (Int32.shift_left !accu 8) (Int32.of_int c)
  done;
  !accu

let input_int64 ch =
  let accu = ref 0L in
  for _i = 0 to 7 do
    let c = input_byte ch in
    accu := Int64.add (Int64.shift_left !accu 8) (Int64.of_int c)
  done;
  !accu

let input_segment_summary ch =
  let nlen = input_int32 ch in
  let name = really_input_string ch (Int32.to_int nlen) in
  let pos = input_int64 ch in
  let len = input_int64 ch in
  let hash = Digest.input ch in
  { name; pos; len; hash }

let rec input_segment_summaries ch n accu =
  if Int32.equal n 0l then List.rev accu
  else
    let s = input_segment_summary ch in
    let accu = (s.name, s) :: accu in
    input_segment_summaries ch (Int32.pred n) accu

let visit_vo f seg =
    let ch = open_in_bin f in
    let magic = input_int32 ch in
    let version = input_int32 ch in
    let summary_pos = input_int64 ch in
    let () = LargeFile.seek_in ch summary_pos in
    let nsum = input_int32 ch in
    let segments = input_segment_summaries ch nsum [] in
    match seg with
    | None ->
      let () = Printf.printf "Coq magic number: %ld\n%!" magic in
      let () = Printf.printf "Coq version: %ld\n%!" version in
      let iter (name, seg) =
        Printf.printf "%s: starting at byte %Li, length %Li [%s]\n" name seg.pos seg.len (Digest.to_hex seg.hash)
      in
      List.iter iter segments
    | Some seg ->
      match List.assoc_opt seg segments with
      | None ->
        let () = Printf.printf "Missing segment \"%s\"\n%!" seg in
        exit 1
      | Some seg ->
        LargeFile.seek_in ch seg.pos;
        let _ = Repr.input ch in
        Repr.dump ()

let usage () =
  let () = Printf.printf "Usage: vodump FILE [SEGMENT]\n%!" in
  exit 1

let () =
  let nargs = Array.length Sys.argv in
  if nargs = 2 then
    let f = Sys.argv.(1) in
    visit_vo f None
  else if nargs = 3 then
    let f = Sys.argv.(1) in
    let n = Sys.argv.(2) in
    visit_vo f (Some n)
  else usage ()
