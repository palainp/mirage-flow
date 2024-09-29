(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix
open Mirage_flow_combinators

let pp_buf ppf buf = Fmt.string ppf (Bytes.to_string buf)
let eq_buf = Bytes.equal

let bytes = Alcotest.testable pp_buf eq_buf
let fail fmt = Fmt.kstr (fun s -> Alcotest.fail s) fmt

let check_buffer = Alcotest.(check bytes)
let check_buffers = Alcotest.(check @@ list bytes)

let check_ok_buffer msg buf = function
  | Ok (`Data b) -> check_buffer msg buf b
  | Ok `Eof      -> fail "%s: eof" msg
  | Error e      -> fail "%s: error=%a" msg F.pp_error e

let check_ok_unit msg = function
  | Ok ()   -> ()
  | Error e -> fail "%s: error=%a" msg F.pp_error e

let check_ok_write msg = function
  | Ok ()   -> ()
  | Error e -> fail "%s: error=%a" msg F.pp_write_error e

let check_closed msg = function
  | Ok ()         -> fail "%s: not closed" msg
  | Error `Closed -> ()
  | Error e       -> fail "%s: error=%a" msg F.pp_write_error e

let check_eof msg = function
  | Ok `Eof -> ()
  | Ok _    -> fail "%s: ok" msg
  | Error e -> fail "%s: error=%a" msg F.pp_error e

let bs str = Bytes.of_string str

let bss = List.map bs

let filter x =
  let zero = Bytes.empty in
  List.filter ((<>) zero) x

let input_string () =
  let input = "xxxxxxxxxx" in
  let ic = F.string ~input () in
  F.read ic >>= fun x1 ->
  F.read ic >>= fun x2 ->
  F.write ic (bs "hihi") >>= fun r ->
  check_ok_buffer "read 1" (bs input) x1;
  check_eof "read 2" x2;
  check_closed "write"  r;
  Lwt.return_unit

let output_string () =
  let output = Bytes.of_string "xxxxxxxxxx" in
  let oc = F.string ~output () in
  F.write oc (bs  "hell") >>= fun x1 ->
  F.write oc (bs   "o! ") >>= fun x2 ->
  F.write oc (bs "world") >>= fun x3 ->
  F.read oc >>= fun r ->
  check_buffer "result" output (bs "hello! wor");
  check_ok_write "write 1" x1;
  check_ok_write "write 2" x2;
  check_closed   "write 3" x3;
  check_eof      "read"    r;
  Lwt.return_unit

let input_strings () =
  let input = [ ""; "123"; "45"; "6789"; "0" ] in
  let ic = F.strings ~input () in
  F.read ic >>= fun x1 ->
  F.read ic >>= fun x2 ->
  F.read ic >>= fun x3 ->
  F.read ic >>= fun x4 ->
  F.read ic >>= fun y ->
  F.read ic >>= fun z ->
  F.write ic (bs "hihi") >>= fun w ->
  check_ok_buffer "read 1" (bs  "123") x1;
  check_ok_buffer "read 2" (bs   "45") x2;
  check_ok_buffer "read 3" (bs "6789") x3;
  check_ok_buffer "read 4" (bs    "0") x4;
  check_eof       "read 5" y;
  check_eof       "read 6" z;
  check_closed    "write"  w;
  Lwt.return_unit

let output_strings () =
  let output = List.map Bytes.of_string ["xxx"; ""; "xx"; "xxx"; ] in
  let oc = F.strings ~output () in
  F.write oc (bs  "hell") >>= fun x1 ->
  F.write oc (bs   "o! ") >>= fun x2 ->
  F.write oc (bs "world") >>= fun x3 ->
  F.read oc >>= fun r ->
  check_buffers "result" (filter output) (bss ["hel"; "lo"; "! w"]);
  check_ok_write "write 1" x1;
  check_ok_write "write 2" x2;
  check_closed   "write 3" x3;
  check_eof     "read"    r;
  Lwt.return_unit

module Lwt_io_flow = Mirage_flow_unix.Make(F)

let input_lwt_io () =
  let ic = F.strings ~input:["1"; "234"; "56"; "78\n90"] () in
  let lic = Lwt_io_flow.ic ic in
  Lwt_io.read_line lic >>= fun l ->
  check_buffer "result" (bs "12345678") (bs l);
  Lwt.return_unit

let output_lwt_io () =
  let output = bss ["xxxx";"xxxx"; "xxxxxx"] in
  let oc = F.strings ~output () in
  let loc = Lwt_io_flow.oc oc in
  Lwt_io.write_line loc "Hello world!" >>= fun () ->
  Lwt_io.flush loc >>= fun () ->
  check_buffers "result" (bss ["Hell"; "o wo"; "rld!\nx"]) output;
  Lwt.return_unit

let run f () = Lwt_main.run (f ())

let string = [
  "input" , `Quick, run input_string;
  "output", `Quick, run output_string;
]

let strings = [
  "input" , `Quick, run input_strings;
  "output", `Quick, run output_strings;
]

let lwt_io = [
  "input" , `Quick, run input_lwt_io;
  "output", `Quick, run output_lwt_io;
]
let () =
  Alcotest.run "mirage-flow" [
    "string"  , string;
    "strings" , strings;
    "lwt-io"  , lwt_io;
  ]
