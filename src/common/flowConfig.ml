(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

let version = "0.27.0"

let default_temp_dir = Filename.concat Sys_utils.temp_dir_name "flow"
let default_shm_dirs =
  try
    Sys.getenv "FLOW_SHMDIR"
    |> Str.(split (regexp ","))
  with _ -> [ "/dev/shm"; default_temp_dir; ]

(* Half a gig *)
let default_shm_min_avail = 1024 * 1024 * 512

let map_add map (key, value) = SMap.add key value map

let multi_error (errs:(int * string) list) =
  let msg =
    errs
    |> List.map (fun (ln, msg) -> spf ".flowconfig:%d %s" ln msg)
    |> String.concat "\n"
  in
  FlowExitStatus.(exit ~msg Invalid_flowconfig)

let error ln msg = multi_error [(ln, msg)]

let project_root_token = Str.regexp_string "<PROJECT_ROOT>";

module Opts = struct
  exception UserError of string

  type moduleSystem = Node | Haste

  type regexp = Str.regexp * string

  type module_name_mapper =
    | NameMapper of regexp * string
    | NameMapperExt of regexp * string

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

  let module_name_mappers opts =
    List.map (function
      | NameMapper ((regexp, _), template) -> regexp, template
      | NameMapperExt ((regexp, _), template) -> regexp, template)
    opts.module_name_mappers

  let suppress_comments opts = List.map fst opts.suppress_comments

  type _initializer =
    | USE_DEFAULT
    | INIT_FN of (t -> t)

  type option_flag =
    | ALLOW_DUPLICATE

  type option_definition = {
    (**
     * The _initializer gets set on the options object immediately before
     * parsing the *first* occurrence of the user-specified config option. This
     * is useful in cases where the user's value should blow away the default
     * value (rather than being aggregated to it).
     *
     * For example: We want the default value of 'module.file_ext' to be
     * ['.js'; '.jsx'], but if the user specifies any 'module.file_ext'
     * settings, we want to start from a clean list.
     *)
    _initializer: _initializer;
    flags: option_flag list;
    optprinter: (t -> string list);
    optparser: (t -> string -> t);
  }

  let get_defined_opts (raw_opts, config) =
    (* If the user specified any options that aren't defined, issue an error *)
    if SMap.cardinal raw_opts > 0 then (
      let errors =
        SMap.elements raw_opts
        |> List.map (fun (k, v) ->
          let msg = spf "Unsupported option specified! (%s)" k in
          List.map (fun (line_num, _) -> (line_num, msg)) v
        )
        |> List.flatten
        |> List.rev
      in
      multi_error errors
    );

    config

  let module_file_exts = SSet.empty
        |> SSet.add ".js"
        |> SSet.add ".jsx"
        |> SSet.add ".json"

  let default_options = {
    enable_const_params = false;
    enable_unsafe_getters_and_setters = false;
    enforce_strict_type_args = true;
    esproposal_class_instance_fields = Options.ESPROPOSAL_WARN;
    esproposal_class_static_fields = Options.ESPROPOSAL_WARN;
    esproposal_decorators = Options.ESPROPOSAL_WARN;
    esproposal_export_star_as = Options.ESPROPOSAL_WARN;
    facebook_ignore_fbt = false;
    ignore_non_literal_requires = false;
    moduleSystem = Node;
    module_name_mappers = [];
    node_resolver_dirnames = ["node_modules"];
    munge_underscores = false;
    module_file_exts;
    modules_are_use_strict = false;
    suppress_comments = [];
    suppress_types = SSet.empty;
    traces = 0;
    strip_root = false;
    all = false;
    log_file = None;
    max_header_tokens = 10;
    max_workers = Sys_utils.nbr_procs;
    temp_dir = default_temp_dir;
    shm_dirs = default_shm_dirs;
    shm_min_avail = default_shm_min_avail;
    shm_dep_table_pow = 17;
    shm_hash_table_pow = 19;
    version = None;
  }

  let parse =
    let parse_line map (line_num, line) =
      if Str.string_match (Str.regexp "^\\([a-zA-Z0-9._]+\\)=\\(.*\\)$") line 0
      then
        let key = Str.matched_group 1 line in
        let value = Str.matched_group 2 line in
        SMap.add key ((line_num, value)::(
          match SMap.get key map with
          | Some values -> values
          | None -> []
        )) map
      else error line_num "Unable to parse line."
    in

    fun config lines ->
      let lines = lines
        |> List.map (fun (ln, line) -> ln, String.trim line)
        |> List.filter (fun (_, s) -> s <> "")
      in
      let raw_options = List.fold_left parse_line SMap.empty lines in
      (raw_options, config)

  let parse_option key definition (raw_opts, config) =
    let new_raw_opts = SMap.remove key raw_opts in

    match SMap.get key raw_opts with
    | None -> (new_raw_opts, config)
    | Some values ->
        let config = (
          match definition._initializer with
          | USE_DEFAULT -> config
          | INIT_FN f ->
              try f config
              with UserError msg ->
                let line_num = fst (List.hd values) in
                error line_num (
                  spf "Error initializing config option \"%s\". %s" key msg
                )
        ) in

        (* Error when duplicate options were incorrectly given *)
        let allow_dupes = List.mem ALLOW_DUPLICATE definition.flags in
        if (not allow_dupes) && (List.length values) > 1 then (
          let line_num = fst (List.nth values 1) in
          error line_num (spf "Duplicate option: \"%s\"" key)
        );

        let config = List.fold_left (fun config (line_num, value_str) ->
          try definition.optparser config value_str
          with UserError msg -> error line_num (
            spf "Error parsing value for \"%s\". %s" key msg
          )
        ) config values in

        (new_raw_opts, config)

  let pretty_print_option ~filter_defaults config key definition raw_opts =
    if SMap.mem key raw_opts
    then failwith (spf "Option %s appears more than once" key);

    let actual_values = definition.optprinter config in
    let default_values = definition.optprinter default_options in
    if filter_defaults && actual_values = default_values
    then raw_opts
    else SMap.add key actual_values raw_opts

  let optprint_enum values getter (config: t) =
    let value = getter config in
    let (key, _) =
      try List.find (fun (_, enum) -> enum = value) values
      with Not_found -> failwith "Can't find value in enum" in
    [ key ]

  let optparse_enum values setter (config: t) str : t =
    let values = List.fold_left map_add SMap.empty values in
    let value = match SMap.get str values with
    | Some v -> v
    | None -> raise (UserError (
        spf "Unsupported value: \"%s\". Supported values are: %s"
          str
          (String.concat ", " (SMap.keys values))
    )) in
    setter config value

  let optprint_boolean, optparse_boolean =
    let values = [
      ("true", true);
      ("false", false);
    ] in
    optprint_enum values, optparse_enum values

  let optprint_uint getter config = [ string_of_int (getter config) ]

  let optparse_uint setter config str =
    let v = int_of_string str in
    if v < 0
    then raise (UserError "Number cannot be negative!")
    else setter config v

  let optprint_string getter config = [ String.escaped (getter config) ]

  let optparse_string setter config str =
    try setter config (Scanf.unescaped str)
    with Scanf.Scan_failure reason -> raise (UserError (
      spf "Invalid ocaml string: %s" reason
    ))

  let optprint_sset getter config =
    SSet.fold (fun str acc -> String.escaped str :: acc) (getter config) []

  let optprint_list optprint getter config =
    List.fold_left
      (fun acc elem ->
        let str_list = optprint (fun _ -> elem) config in
        str_list @ acc)
      []
      (getter config)
    |> List.rev

  let optprint_regexp getter config = [String.escaped (snd (getter config))]

  let optparse_regexp setter config str =
    let unescaped = Scanf.unescaped str in
    try setter config (Str.regexp unescaped, unescaped)
    with Failure reason -> raise (UserError (
      spf "Invalid regex \"%s\" (%s)" unescaped reason
    ))

  let esproposal_feature_flag_values ~allow_enable =
    let values = [
      ("ignore", Options.ESPROPOSAL_IGNORE);
      ("warn", Options.ESPROPOSAL_WARN);
    ] in
    if allow_enable
    then ("enable", Options.ESPROPOSAL_ENABLE)::values
    else values

  let optprint_esproposal_feature_flag ?(allow_enable=false) =
    optprint_enum (esproposal_feature_flag_values ~allow_enable)

  let optparse_esproposal_feature_flag ?(allow_enable=false) =
    optparse_enum (esproposal_feature_flag_values ~allow_enable)

  let optprint_option optprint getter config =
    match getter config with
    | None -> []
    | Some opt -> optprint (fun _ -> opt) config

  let optprint_filepath getter config = [ Path.to_string (getter config) ]

  let optparse_filepath setter config str = setter config (Path.make str)
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

let empty_config = {
  ignores = [];
  includes = [];
  libs = [];
  options = Opts.default_options;
}

let group_into_sections lines =
  let is_section_header = Str.regexp "^\\[\\(.*\\)\\]$" in
  let _, sections, section =
    List.fold_left (fun (seen, sections, (section, lines)) (ln, line) ->
      if Str.string_match is_section_header line 0
      then begin
        let sections = (section, List.rev lines)::sections in
        let section_name = Str.matched_group 1 line in
        if SSet.mem section_name seen
        then error ln (spf "contains duplicate section: \"%s\"" section_name);
        SSet.add section_name seen, sections, ((ln, section_name), [])
      end else
        seen, sections, (section, (ln, line)::lines)
    ) (SSet.empty, [], ((0, ""), [])) lines in
  let (section, section_lines) = section in
  List.rev ((section, List.rev section_lines)::sections)

let trim_lines lines =
  lines
  |> List.map (fun (_, line) -> String.trim line)
  |> List.filter (fun s -> s <> "")

(* parse [include] lines *)
let parse_includes config lines =
  let includes = trim_lines lines in
  { config with includes; }

let parse_libs config lines =
  let libs = trim_lines lines in
  { config with libs; }

let parse_ignores config lines =
  let ignores = trim_lines lines in
  { config with ignores; }

let fold_definitions ~f ~init =
  let open Opts in
  init
  |> f "esproposal.class_instance_fields" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_esproposal_feature_flag ~allow_enable:true
      (fun opts -> opts.esproposal_class_instance_fields);
    optparser = optparse_esproposal_feature_flag ~allow_enable:true
      (fun opts v -> { opts with esproposal_class_instance_fields = v; });
  }

  |> f "esproposal.class_static_fields" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_esproposal_feature_flag ~allow_enable:true
      (fun opts -> opts.esproposal_class_static_fields);
    optparser = optparse_esproposal_feature_flag ~allow_enable:true
      (fun opts v -> { opts with esproposal_class_static_fields = v; });
  }

  |> f "esproposal.decorators" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_esproposal_feature_flag
      (fun opts -> opts.esproposal_decorators);
    optparser = optparse_esproposal_feature_flag
      (fun opts v -> { opts with esproposal_decorators = v; });
  }

  |> f "esproposal.export_star_as" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_esproposal_feature_flag ~allow_enable:true
      (fun opts -> opts.esproposal_export_star_as);
    optparser = optparse_esproposal_feature_flag ~allow_enable:true
      (fun opts v -> { opts with esproposal_export_star_as = v; });
  }

  |> f "facebook.ignore_fbt" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_boolean
      (fun opts -> opts.facebook_ignore_fbt);
    optparser = optparse_boolean
      (fun opts v -> { opts with facebook_ignore_fbt = v; });
  }

  |> f "log.file" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_option optprint_filepath
      (fun opts -> opts.log_file);
    optparser = optparse_filepath
      (fun opts v -> { opts with log_file = Some v; });
  }

  |> f "max_header_tokens" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_uint
      (fun opts -> opts.max_header_tokens);
    optparser = optparse_uint
      (fun opts max_header_tokens -> {opts with max_header_tokens;});
  }

  |> f "module.ignore_non_literal_requires" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_boolean
      (fun opts -> opts.ignore_non_literal_requires);
    optparser = optparse_boolean
      (fun opts v -> {opts with ignore_non_literal_requires = v;});
  }

  |> f "module.file_ext" {
    _initializer = INIT_FN (fun opts -> {
      opts with module_file_exts = SSet.empty;
    });
    flags = [ALLOW_DUPLICATE];
    optprinter = optprint_sset
      (fun opts -> opts.module_file_exts);
    optparser = optparse_string
      (fun opts v ->
        if Utils.str_ends_with v Files_js.flow_ext
        then raise (Opts.UserError (
          "Cannot use file extension '" ^
          v ^
          "' since it ends with the reserved extension '"^
          Files_js.flow_ext^
          "'"
        ));
        let module_file_exts = SSet.add v opts.module_file_exts in
        {opts with module_file_exts;}
      );
  }

  |> f "module.name_mapper" {
    _initializer = USE_DEFAULT;
    flags = [ALLOW_DUPLICATE];
    optprinter = (fun opts ->
      List.fold_left (fun acc elem -> match elem with
        | NameMapper ((_, unescaped), template) ->
            spf "%s -> %s" unescaped template :: acc
        | _ -> acc) [] (List.rev opts.module_name_mappers)
    );
    optparser = (fun opts str ->
      let regexp_str = "^'\\([^']*\\)'[ \t]*->[ \t]*'\\([^']*\\)'$" in
      let regexp = Str.regexp regexp_str in
      (if not (Str.string_match regexp str 0) then
        raise (Opts.UserError (
          "Expected a mapping of form: " ^
          "'single-quoted-string' -> 'single-quoted-string'"
        ))
      );

      let pattern = Str.matched_group 1 str in
      let template = Str.matched_group 2 str in

      let v = NameMapper ((Str.regexp pattern, pattern), template) in
      let module_name_mappers = v :: opts.module_name_mappers in
      {opts with module_name_mappers;}
    );
  }

  |> f "module.name_mapper.extension" {
    _initializer = USE_DEFAULT;
    flags = [ALLOW_DUPLICATE];

    optprinter = (fun opts ->
      List.fold_left (fun acc elem -> match elem with
        | NameMapperExt ((_, unescaped), template) ->
            spf "%s -> %s" unescaped template :: acc
        | _ -> acc) [] (List.rev opts.module_name_mappers)
    );

    optparser = (fun opts str ->
      let regexp_str = "^'\\([^']*\\)'[ \t]*->[ \t]*'\\([^']*\\)'$" in
      let regexp = Str.regexp regexp_str in
      (if not (Str.string_match regexp str 0) then
        raise (Opts.UserError (
          "Expected a mapping of form: " ^
          "'single-quoted-string' -> 'single-quoted-string'"
        ))
      );

      let file_ext = Str.matched_group 1 str in
      let template = Str.matched_group 2 str in

      let regexp = "^\\(.*\\)\\." ^ (Str.quote file_ext) ^ "$" in
      let v = NameMapperExt ((Str.regexp regexp, regexp), template) in
      let module_name_mappers = v :: opts.module_name_mappers in
      {opts with module_name_mappers;}
    );
  }

  |> f "module.system" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_enum [
      ("node", Node);
      ("haste", Haste);
    ] (fun opts -> opts.moduleSystem);
    optparser = optparse_enum [
      ("node", Node);
      ("haste", Haste);
    ] (fun opts v -> { opts with moduleSystem = v; });
  }

  |> f "module.system.node.resolve_dirname" {
    _initializer = INIT_FN (fun opts -> {
      opts with node_resolver_dirnames = [];
    });
    flags = [ALLOW_DUPLICATE];
    optprinter = optprint_list optprint_string
      (fun opts -> opts.node_resolver_dirnames);
    optparser = optparse_string
      (fun opts v ->
        let node_resolver_dirnames = v :: opts.node_resolver_dirnames in
        {opts with node_resolver_dirnames;}
      );
  }

  |> f "module.use_strict" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_boolean
      (fun opts -> opts.modules_are_use_strict);
    optparser = optparse_boolean
      (fun opts v -> {opts with modules_are_use_strict = v;} );
  }

  |> f "munge_underscores" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_boolean
      (fun opts -> opts.munge_underscores);
    optparser = optparse_boolean
      (fun opts v -> {opts with munge_underscores = v;} );
  }

  |> f "server.max_workers" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_uint
      (fun opts -> opts.max_workers);
    optparser = optparse_uint
      (fun opts v -> {opts with max_workers = v;});
  }

  |> f "strip_root" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_boolean
      (fun opts -> opts.strip_root);
    optparser = optparse_boolean
      (fun opts v -> {opts with strip_root = v;});
  }

  |> f "all" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_boolean
      (fun opts -> opts.all);
    optparser = optparse_boolean
      (fun opts v -> {opts with all = v;});
  }

  |> f "suppress_comment" {
    _initializer = USE_DEFAULT;
    flags = [ALLOW_DUPLICATE];
    optprinter = optprint_list optprint_regexp
      (fun opts -> opts.suppress_comments);
    optparser = optparse_regexp
      (fun opts v ->
        {opts with suppress_comments = v::(opts.suppress_comments);}
      );
  }

  |> f "suppress_type" {
    _initializer = USE_DEFAULT;
    flags = [ALLOW_DUPLICATE];
    optprinter = optprint_sset
      (fun opts -> opts.suppress_types);
    optparser = optparse_string
      (fun opts v -> {
        opts with suppress_types = SSet.add v opts.suppress_types;
      });
  }

  |> f "temp_dir" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_string
      (fun opts -> opts.temp_dir);
    optparser = optparse_string
      (fun opts v -> { opts with temp_dir = v; });
  }

  |> f "sharedmemory.dirs" {
    _initializer = USE_DEFAULT;
    flags = [ALLOW_DUPLICATE];
    optprinter = optprint_list optprint_string
      (fun opts -> opts.shm_dirs);
    optparser = optparse_string
      (fun opts v -> { opts with shm_dirs = opts.shm_dirs @ [v]; });
  }

  |> f "sharedmemory.minimum_available" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_uint
      (fun opts -> opts.shm_min_avail);
    optparser = optparse_uint
      (fun opts shm_min_avail -> { opts with shm_min_avail; });
  }

  |> f "sharedmemory.dep_table_pow" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_uint
      (fun opts -> opts.shm_dep_table_pow);
    optparser = optparse_uint
      (fun opts shm_dep_table_pow -> { opts with shm_dep_table_pow; });
  }

  |> f "sharedmemory.hash_table_pow" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_uint
      (fun opts -> opts.shm_hash_table_pow);
    optparser = optparse_uint
      (fun opts shm_hash_table_pow -> { opts with shm_hash_table_pow; });
  }

  |> f "traces" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_uint
      (fun opts -> opts.traces);
    optparser = optparse_uint
      (fun opts v -> {opts with traces = v;});
  }

  |> f "unsafe.enable_getters_and_setters" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_boolean
      (fun opts -> opts.enable_unsafe_getters_and_setters);
    optparser = optparse_boolean
      (fun opts v -> {opts with enable_unsafe_getters_and_setters = v;});
  }

  |> f "experimental.const_params" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_boolean
      (fun opts -> opts.enable_const_params);
    optparser = optparse_boolean
      (fun opts v -> {opts with enable_const_params = v;});
  }

  |> f "experimental.strict_type_args" {
    _initializer = USE_DEFAULT;
    flags = [];
    optprinter = optprint_boolean
      (fun opts -> opts.enforce_strict_type_args);
    optparser = optparse_boolean
      (fun opts v -> {opts with enforce_strict_type_args = v;});
  }

let parse_options config lines =
  let open Opts in
  let options = fold_definitions 
    ~f:parse_option
    ~init:(Opts.parse config.options lines)
  |> get_defined_opts
  in
  {config with options}

let pretty_print_options ~filter_defaults config =
  let open Opts in
  fold_definitions 
    ~f:(pretty_print_option ~filter_defaults config) 
    ~init:SMap.empty

let parse_version config lines =
  let potential_versions = lines
  |> List.map (fun (ln, line) -> ln, String.trim line)
  |> List.filter (fun (_, s) -> s <> "")
  in

  match potential_versions with
  | (ln, version_str) :: _ ->
    if not (Semver.is_valid_range version_str) then
      error ln (
        spf
          "Expected version to match %%d.%%d.%%d, with an optional leading ^, got %s"
          version_str
      );

    let options = { config.options with Opts.version = Some version_str } in
    { config with options }
  | _ -> config


let parse_section config ((section_ln, section), lines) =
  match section, lines with
  | "", [] when section_ln = 0 -> config
  | "", (ln, _)::_ when section_ln = 0 ->
      error ln "Unexpected config line not in any section"
  | "include", _ -> parse_includes config lines
  | "ignore", _ -> parse_ignores config lines
  | "libs", _ -> parse_libs config lines
  | "options", _ -> parse_options config lines
  | "version", _ -> parse_version config lines
  | _ -> error section_ln (spf "Unsupported config section: \"%s\"" section)

let parse config lines =
  let sections = group_into_sections lines in
  List.fold_left parse_section config sections

let is_not_comment =
  let comment_regexps = [
    Str.regexp_string "#";                (* Line starts with # *)
    Str.regexp_string ";";                (* Line starts with ; *)
    Str.regexp_string "\240\159\146\169"; (* Line starts with poop emoji *)
  ] in
  fun (_, line) ->
    not (List.exists
      (fun (regexp) -> Str.string_match regexp line 0)
      comment_regexps)

let read filename =
  let lines = Sys_utils.cat_no_fail filename
    |> Sys_utils.split_lines
    |> List.mapi (fun i line -> (i+1, String.trim line))
    |> List.filter is_not_comment in
  parse empty_config lines

let init ~ignores ~includes ~libs ~options =
  let ignores_lines = List.map (fun s -> (1, s)) ignores in
  let includes_lines = List.map (fun s -> (1, s)) includes in
  let options_lines = List.map (fun s -> (1, s)) options in
  let lib_lines = List.map (fun s -> (1, s)) libs in
  let config = parse_ignores empty_config ignores_lines in
  let config = parse_includes config includes_lines in
  let config = parse_options config options_lines in
  let config = parse_libs config lib_lines in
  config

(* We should restart every time the config changes, so it's cool to cache it *)
let cache = ref None

let get filename =
  match !cache with
  | None ->
      let config = read filename in
      cache := Some (filename, config);
      config
  | Some (cached_filename, config) ->
      assert (filename = cached_filename);
      config

let restore (filename, config) = cache := Some (filename, config)

module Pp : sig
  val flowconfig : json:bool -> config -> string
  val excerpt : json:bool -> string -> config -> string
  val options_lines : config -> string list
end = struct
  open Hh_json

  type config_part =
    | Sections of (string * config_part) list
    | Lines of string list
    | KeyValues of (string * string list) list
    | Empty

  let ignores ignores =
    ("ignore", (Lines ignores))

  let includes includes =
    ("include", (Lines includes))

  let libs libs =
    ("lib", (Lines libs))

  let options options =
    let opts = pretty_print_options ~filter_defaults:true options
    |> SMap.elements in
    ("options", KeyValues opts)

  let version version =
    match version with
    | None -> ("version", Empty)
    | Some version -> ("version", Lines [version])

  let is_nonempty_section = function
  | (_, Empty)
  | (_, Lines []) -> false
  | _ -> true

  let sections config =
    Sections (
      [
        ignores config.ignores;
        includes config.includes;
        libs config.libs;
        options config.options;
        version config.options.Opts.version;
      ]
      |> List.filter is_nonempty_section
    )
    
  let rec flowconfig_to_string config_part =
    match config_part with
    | Sections sections ->
        sections
        |> List.map (fun (name, config_part) ->
          spf "[%s]\n%s" name (flowconfig_to_string config_part))
        |> String.concat "\n\n"
    | Lines values -> String.concat "\n" values
    | KeyValues values ->
        values
        |> List.map (fun (key, values) -> 
            values
            |> List.map (fun value ->  spf "%s=%s" key value)
            |> String.concat "\n")
        |> String.concat "\n"
    | Empty -> ""

  let rec flowconfig_to_json config_part =
    match config_part with
    | Sections sections ->
        JSON_Object (
          List.map (fun (name, config_part) ->
            (name, flowconfig_to_json config_part)
          ) sections
        )
    | Lines values -> 
        JSON_Array (List.map (fun value -> JSON_String value) values)
    | KeyValues values ->
        JSON_Object (
          List.map (fun (key, values) -> 
            (key, JSON_Array (List.map (fun v -> JSON_String v) values))
          ) values
        )
    | Empty -> JSON_Null

  let to_string ~json config_part =
    if json 
    then json_to_string (flowconfig_to_json config_part)
    else (flowconfig_to_string config_part)^"\n"

  let flowconfig ~json config =
    to_string ~json (sections config)

  let excerpt ~json key config =
    begin match key with
    | "ignore" -> Sections [ ignores config.ignores ]
    | "include" -> Sections [ includes config.includes ]
    | "libs" -> Sections [ libs config.libs ]
    | "options" -> Sections [ options config.options ]
    | "version" -> Sections [ version config.options.Opts.version]
    | key -> 
        let opts = pretty_print_options ~filter_defaults:false config.options in
        begin match SMap.get key opts with
        | None ->
            FlowExitStatus.(exit
              ~msg:(spf "Unknown option: %s" key)
              Input_error)
        | Some opts ->
            KeyValues [(key, opts)]
        end
    end
    |> to_string ~json 
    
  let options_lines config = 
    pretty_print_options ~filter_defaults:true config.options
    |> SMap.elements
    |> List.map (fun (key, values) -> 
        List.map (fun value -> spf "%s=%s" key value) values
      )
    |> List.concat
end

let string_of_config ?(json=false) ?key config =
  match key with
  | None -> Pp.flowconfig ~json config
  | Some key -> Pp.excerpt ~json key config


let add ~key ~value config =
  let value = String.escaped value in
  match key with
  | "ignore" -> 
      let ignores = config.ignores @ [ value ] in
      { config with ignores; }
  | "include" -> 
      let includes = config.includes @ [ value ] in
      { config with includes; }
  | "libs" -> 
      let libs = config.libs @ [ value ] in
      { config with libs; }
  | "options" ->
      let option_lines = (Pp.options_lines config) @ [value] in
      let option_lines = List.mapi (fun idx v -> (idx+1, v)) option_lines in
      let config = { config with options = Opts.default_options } in
      parse_options config option_lines 
  | "version" ->
      begin match config.options.Opts.version with
      | None -> 
          parse_version config [ (1, value) ]
      | Some _ ->
          FlowExitStatus.(exit
            ~msg:"Cannot add version since a version is already specified. \
                  Did you mean to use --replace-all?"
            Input_error)
      end
  | key ->
      let option_lines = 
        (Pp.options_lines config) @ [ spf "%s=%s" key value ] in
      let option_lines = List.mapi (fun idx v -> (idx+1, v)) option_lines in
      let config = { config with options = Opts.default_options } in
      parse_options config option_lines 
