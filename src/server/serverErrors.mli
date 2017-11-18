(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type errors = {
  (* errors are stored in a map from file path to error set, so that the errors
     from checking particular files can be cleared during recheck. *)
  local_errors: Errors.ErrorSet.t Utils_js.FilenameMap.t;
  (* errors encountered during merge have to be stored separately so
     dependencies can be cleared during merge. *)
  merge_errors: Errors.ErrorSet.t Utils_js.FilenameMap.t;
  (* error suppressions in the code *)
  suppressions: Error_suppressions.t Utils_js.FilenameMap.t;
  (* lint severity settings in the code *)
  severity_cover_set: ExactCover.lint_severity_cover Utils_js.FilenameMap.t;
}

type collated_errors = {
  collated_errorset: Errors.ErrorSet.t;
  collated_warning_map: Errors.ErrorSet.t Utils_js.FilenameMap.t;
  collated_suppressed_errors: (Errors.error * Loc.LocSet.t) list;
}

val clear_files: FilenameSet.t -> unit
val report_local_errors: Errors.ErrorSet.t Utils_js.FilenameMap.t -> unit
val report_merge_errors: File_key.t -> Errors.ErrorSet.t -> unit
val finalize: unit -> unit

val get_with_separate_warnings:
  checked_files:CheckedSet.t ->
  t ->
  Errors.ErrorSet.t * Errors.ErrorSet.t Utils_js.FilenameMap.t * (Errors.error * Loc.LocSet.t) list

val get:
  checked_files:CheckedSet.t ->
  t ->
  Errors.ErrorSet.t * Errors.ErrorSet.t * (Errors.error * Loc.LocSet.t) list
