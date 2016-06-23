(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Opts : sig
  type moduleSystem = Node | Haste

  type module_name_mapper
  type regexp

  type t = {
    enable_const_params: bool;
    enable_unsafe_getters_and_setters: bool;
    enforce_strict_type_args: bool;
    esproposal_class_instance_fields: Options.esproposal_feature_mode;
    esproposal_class_static_fields: Options.esproposal_feature_mode;
    esproposal_decorators: Options.esproposal_feature_mode;
    esproposal_export_star_as: Options.esproposal_feature_mode;
    facebook_ignore_fbt: bool;
    ignore_non_literal_requires: bool;
    moduleSystem: moduleSystem;
    module_name_mappers: module_name_mapper list;
    node_resolver_dirnames: string list;
    munge_underscores: bool;
    module_file_exts: SSet.t;
    modules_are_use_strict: bool;
    suppress_comments: regexp list;
    suppress_types: SSet.t;
    traces: int;
    strip_root: bool;
    all: bool;
    log_file: Path.t option;
    max_header_tokens: int;
    max_workers: int;
    temp_dir: string;
    shm_dirs: string list;
    shm_min_avail: int;
    shm_dep_table_pow: int;
    shm_hash_table_pow: int;
    version: string option;
  }

  val module_name_mappers: t -> (Str.regexp * string) list
  val suppress_comments: t -> Str.regexp list
end

type config = {
  (* file blacklist *)
  ignores: string list;
  (* non-root include paths *)
  includes: string list;
  (* library paths. no wildcards *)
  libs: string list;
  (* config options *)
  options: Opts.t;
}

val get: string -> config

val init:
  ignores: string list ->
  includes: string list ->
  libs: string list ->
  options: string list ->
  config

val string_of_config: ?json:bool -> ?key:string -> config -> string

val add: key:string -> value:string -> config -> config

val version: string
val project_root_token: Str.regexp

val restore: string * config -> unit
