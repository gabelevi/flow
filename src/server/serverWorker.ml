(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let builder =
  let save () =
    Files_js.get_lib_files (),
    Flow_js.builtins (),
    Flow_js.master_cx (),
    Relative_path.save (),
    FlowConfig.get_unsafe () in
  let restore (lf, b, cx, rp, fc) =
    Files_js.restore_lib_files lf;
    Flow_js.restore_builtins b;
    Flow_js.restore_master_cx cx;
    Relative_path.restore rp;
    FlowConfig.restore fc in
  Worker.register_state_handler { Worker.save; restore; }

let make options handle =
  let gc_control   = GlobalConfig.gc_control in
  let nbr_procs    = GlobalConfig.nbr_procs in
  Worker.make builder gc_control handle nbr_procs
