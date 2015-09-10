(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Module building workers.
 * A worker is a subprocess executing an arbitrary function.
 * You should first create a fixed amount of workers and then use those 
 * because the amount of workers is limited and to make the load-balancing
 * of tasks better (cf multiWorker.ml).
 *)
(*****************************************************************************)

(* The type of a worker visible to the outside world *)
type t

(*****************************************************************************)
(* The handle is what we get back when we start a job. It's a "future"
 * (sometimes called a "promise"). The scheduler uses the handle to retrieve
 * the result of the job when the task is done (cf multiWorker.ml).
 * Note that the scheduler has to use a handle for that. But the handle
 * is just a trick to get type-checking on workers, a handle is a
 * phantom type, it doesn't really have a value.
 *)
(*****************************************************************************)
type 'a handle

type builder
type 'a state_handler = {
  restore: 'a -> unit;
  save: unit -> 'a;
}

(* This function create an alternate entry points and its usage should
   follow the same rules than `Daemon.register_entry_point`. *)
val register_state_handler: 'a state_handler -> builder

(* Creates a worker *)
val make: builder -> Gc.control -> SharedMem.handle -> int -> t list

(* Call in a sub-process (CAREFUL, GLOBALS ARE COPIED) *)
val call: t -> ('a -> 'b) -> 'a -> 'b handle

(* Retrieves the result (once the worker is done) hangs otherwise *)
val get_result: 'a handle -> 'a

(* Selects among multiple process those which are ready *)
type 'a selected = {
  readys: 'a handle list;
  waiters: 'a handle list;
}
val select: 'a handle list -> 'a selected

val get_worker: 'a handle -> t

val killall: unit -> unit
