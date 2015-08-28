(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let builder =
  Worker.register_state_handler {
    Worker.save = Relative_path.save;
    restore = Relative_path.restore;
  }

let make options config handle =
  let gc_control = ServerConfig.gc_control config in
  let nbr_procs  = GlobalConfig.nbr_procs in
  Some (Worker.make builder gc_control handle nbr_procs)
