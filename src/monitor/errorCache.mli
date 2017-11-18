(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val clear: unit -> unit Lwt.t
val add: errors:Errors.ErrorSet.t -> warnings:Errors.ErrorSet.t -> unit Lwt.t
val finalize: unit -> unit Lwt.t

val get: unit -> (Errors.ErrorSet.t * Errors.ErrorSet.t) Lwt.t
