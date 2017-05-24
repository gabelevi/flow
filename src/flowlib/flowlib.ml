(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let write_flowlib dir (filename, contents) =
  let file = Path.(concat dir filename |> to_string) in
  Sys_utils.write_file ~file contents

let extract_flowlib dir =
  List.iter (write_flowlib dir) Flowlib_contents.contents
