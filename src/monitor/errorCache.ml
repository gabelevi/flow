(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ESet = Errors.ErrorSet

type state =
| Partial
| Final

type cache = {
  errors: ESet.t;
  warnings: ESet.t;
  state: state;
}

let empty = {
  errors = ESet.empty;
  warnings = ESet.empty;
  state = Partial;
}

let mutex = Lwt_mutex.create ()

let cache = ref empty

let clear () = Lwt_mutex.with_lock mutex (fun () ->
  cache := empty;
  Lwt.return_unit
)

let add ~errors:new_errors ~warnings:new_warnings = Lwt_mutex.with_lock mutex (fun () ->
  let { errors=old_errors; warnings=old_warnings; state } = !cache in
  if state <> Partial then failwith "Adding errors to final state!";
  cache := {
    errors = ESet.union old_errors new_errors;
    warnings = ESet.union old_warnings new_warnings;
    state;
  };
  Lwt.return_unit
)

let finalize () = Lwt_mutex.with_lock mutex (fun () ->
  cache := { !cache with state = Final };
  Lwt.return_unit
)

let get () = Lwt_mutex.with_lock mutex (fun () ->
  let { errors; warnings; state; } = !cache in
  if state <> Partial then failwith "Error cache already finalize!";
  Lwt.return (errors, warnings)
)
