(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* The "static" environment, initialized first and then doesn't change *)
(*****************************************************************************)

type genv = {
    options          : Options.t;
    workers          : Worker.t list option;
  }

(*****************************************************************************)
(* The environment constantly maintained by the server *)
(*****************************************************************************)

type env = {
    (* All the files that we at least parse. *)
    files: Utils_js.FilenameSet.t;
    (* All the current files we typecheck. *)
    checked_files: CheckedSet.t;
    libs: SSet.t; (* a subset of `files` *)
    errors: ServerErrors.t;
    connections: Persistent_connection.t;
}
