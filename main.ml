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
    let ans = String.concat " " !ans in
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
        Printf.printf "%0*x::B:%i[%i]{%s}\n%!" shf i tag (Array.length obj) (pr_struct shf obj)
      | Int64 n ->
        Printf.printf "%0*x::I:%s\n%!" shf i (Int64.to_string n)
      | Float64 f ->
        Printf.printf "%0*x::F:%s\n%!" shf i (string_of_float f)
      | String s ->
        Printf.printf "%0*x::S:\"%s\"\n%!" shf i (String.escaped s)
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
  pos : int;
  header : header;
}

let parse_segment ch =
  let pos = input_binary_int ch in
  let org = pos_in ch in
  let header = parse_header ch in
  let seg = { pos = org; header } in
  seek_in ch pos;
  ignore(Digest.input ch);
  seg

let rec parse_segments ch accu = match parse_segment ch with
| seg -> parse_segments ch (seg :: accu)
| exception _ -> accu

let visit_vo f seg =
    let ch = open_in_bin f in
    let magic = input_binary_int ch in
    let segments = parse_segments ch [] in
    let segments = Array.of_list (List.rev segments) in
    match seg with
    | None ->
      Printf.printf "Coq magic number: %d\n%!" magic;
      Array.iteri (fun i { pos; header } ->
        let size = if Sys.word_size = 64 then header.size64 else header.size32 in
        Printf.printf "%d: starting at byte %d (size %iw)\n" i pos size)
        segments
    | Some seg ->
       seek_in ch segments.(seg).pos;
       let _ = Repr.input ch in
       Repr.dump ()

let usage () =
  let () = Printf.printf "Usage: vodump FILE [NUMBER]\n%!" in
  exit 1

let () =
  let nargs = Array.length Sys.argv in
  if nargs = 2 then
    let f = Sys.argv.(1) in
    visit_vo f None
  else if nargs = 3 then
    let f = Sys.argv.(1) in
    let n = try int_of_string Sys.argv.(2) with _ -> usage () in
    visit_vo f (Some n)
  else usage ()
