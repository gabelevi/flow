(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let get_flowlibs dir =
  Sys.readdir dir
  |> Array.fold_left (fun acc file ->
     let contents = Sys_utils.read_file (Filename.concat dir file) in
     (file, contents)::acc
  ) []

let generate_flowlibs_file out_file contents =
  let compressed = LZ4.compress_string (Marshal.to_bytes contents []) in
  let oc = open_out out_file in
  Printf.fprintf oc "let contents = %S;\n" compressed;
  close_out oc


let () =
  let flowlib_dir = Sys.argv.(1) in
  let out_file = Sys.argv.(2) in
  let flowlib_contents = get_flowlibs flowlib_dir in
  generate_flowlibs_file out_file flowlib_contents
