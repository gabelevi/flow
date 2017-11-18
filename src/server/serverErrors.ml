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

type state = {
  mutable errors: errors;
  mutable collated_errors: collated_errors;
  mutable typecheck_in_progress: bool;
}

let clear_files files =
  state.errors <- FilenameSet.fold
    (fun file { local_errors; merge_errors; suppressions; severity_cover_set; } ->
      Hh_logger.debug "clear errors %s" (File_key.to_string file);
      {
        local_errors = FilenameMap.remove file local_errors;
        merge_errors = FilenameMap.remove file merge_errors;
        suppressions = FilenameMap.remove file suppressions;
        severity_cover_set = FilenameMap.remove file severity_cover_set;
      }
    ) files state.errors
    
val report_local_errors: Errors.ErrorSet.t Utils_js.FilenameMap.t -> unit
val report_merge_errors: File_key.t -> Errors.ErrorSet.t -> unit

let finalize () = state.typecheck_in_progress <- true


(* combine error maps into a single error set and a filtered warning map
 *
 * This can be a little expensive for large repositories and can take a couple of seconds.
 * Therefore there are a few things we want to do:
 *
 * 1. Memoize the result in env. This means subsequent calls to commands like `flow status` can
 *    be fast
 * 2. Eagerly calculate `collate_errors` after init or a recheck, so that the server still has
 *    the init or recheck lock. If we improve how clients can tell if a server is busy or stuck
 *    then we can probably relax this.
 * 3. Throw away the collated errors when lazy mode's typecheck_contents adds more dependents or
 *    dependencies to the checked set
 **)
let regenerate =
  let open Errors in
  let open Error_suppressions in
  let open Utils_js in
  let add_unused_suppression_warnings checked suppressions warnings =
    (* For each unused suppression, create an warning *)
    Error_suppressions.unused suppressions
    |> List.fold_left
      (fun warnings loc ->
        let source_file = match Loc.source loc with Some x -> x | None -> File_key.SourceFile "-" in
        (* In lazy mode, dependencies are modules which we typecheck not because we care about
         * them, but because something important (a focused file or a focused file's dependent)
         * needs these dependencies. Therefore, we might not typecheck a dependencies' dependents.
         *
         * This means there might be an unused suppression comment warning in a dependency which
         * only shows up in lazy mode. To avoid this, we'll just avoid raising this kind of
         * warning in any dependency.*)
        if not (CheckedSet.dependencies checked |> FilenameSet.mem source_file)
        then begin
          let err =
            let msg = Flow_error.EUnusedSuppression loc in
            Flow_error.error_of_msg ~trace_reasons:[] ~op:None ~source_file msg in
          let file_warnings = FilenameMap.get source_file warnings
            |> Option.value ~default:ErrorSet.empty
            |> ErrorSet.add err in
          FilenameMap.add source_file file_warnings warnings
        end else
          warnings
      )
      warnings
  in
  let acc_fun severity_cover filename file_errs
      (errors, warnings, suppressed_errors, suppressions) =
    let file_errs, file_warns, file_suppressed_errors, suppressions =
      filter_suppressed_errors suppressions severity_cover file_errs in
    let errors = ErrorSet.union file_errs errors in
    let warnings = FilenameMap.add filename file_warns warnings in
    let suppressed_errors = List.rev_append file_suppressed_errors suppressed_errors in
    (errors, warnings, suppressed_errors, suppressions)
  in
  fun ~checked_files ~errors:{ local_errors; merge_errors; suppressions; severity_cover_set; } ->
    let suppressions = union_suppressions suppressions in

    (* union the errors from all files together, filtering suppressed errors *)
    let severity_cover = ExactCover.union_all severity_cover_set in
    let acc_fun = acc_fun severity_cover in
    let collated_errorset, warnings, collated_suppressed_errors, suppressions =
      (ErrorSet.empty, FilenameMap.empty, [], suppressions)
      |> FilenameMap.fold acc_fun local_errors
      |> FilenameMap.fold acc_fun merge_errors
    in

    let collated_warning_map =
      add_unused_suppression_warnings checked_files suppressions warnings in
    { collated_errorset; collated_warning_map; collated_suppressed_errors }


let get_with_separate_warnings ~checked_files t =
  let collated_errors = match t.collated_errors with
  | None ->
    let collated_errors = regenerate ~checked_files ~errors:t.errors in
    t.collated_errors <- Some collated_errors;
    collated_errors
  | Some collated_errors ->
    collated_errors
  in
  let { collated_errorset; collated_warning_map; collated_suppressed_errors } = collated_errors in
  (collated_errorset, collated_warning_map, collated_suppressed_errors)

(* combine error maps into a single error set and a single warning set *)
let get ~checked_files t =
  let open Errors in
  let errors, warning_map, suppressed_errors = get_with_separate_warnings ~checked_files t in
  let warnings =
    Utils_js.FilenameMap.fold (fun _key -> ErrorSet.union) warning_map ErrorSet.empty in
  (errors, warnings, suppressed_errors)
