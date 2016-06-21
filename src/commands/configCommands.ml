module Init = struct
  let spec = {
    CommandSpec.
    name = "init";
    doc = "Initializes a directory to be used as a flow root directory";
    usage = Printf.sprintf
      "Usage: %s init [ROOT]\n\
        Initializes a directory to be used as a flow root directory\n\n\
        e.g. %s init /path/to/root\n\
        or %s init\n\
        or %s init --options \"optionA=123;optionB=456\"\n\n\
        If the root is not specified it is assumed to be the current working directory\n\n\
        This command will create and initialize /path/to/root/.flowconfig\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args = CommandSpec.ArgSpec.(
      empty
      |> CommandUtils.from_flag
      |> CommandUtils.flowconfig_flags
      |> flag "--options" (optional string)
          ~doc:"Semicolon-delimited list of key=value pairs"
      |> anon "root" (optional string)
          ~doc:"Root directory (default: current working directory)"
    )
  }

  let main from flowconfig_flags options root () =
    FlowEventLogger.set_from from;
    let root = match root with
    | None -> Sys.getcwd () |> Path.make
    | Some root -> Path.make root
    in
    let options = match options with
    | None -> []
    | Some str -> Str.split (Str.regexp ";") str
    in
    let ignores = flowconfig_flags.CommandUtils.ignores in
    let includes = flowconfig_flags.CommandUtils.includes in
    let libs = flowconfig_flags.CommandUtils.libs in

    let file = Server_files_js.config_file root in
    if Sys.file_exists file
    then begin
      let msg = Utils_js.spf "Error: \"%s\" already exists!\n%!" file in
      FlowExitStatus.(exit ~msg Invalid_flowconfig)
    end;

    let config = FlowConfig.init ~ignores ~includes ~libs ~options in

    let out = Sys_utils.open_out_no_fail file in
    output_string out (FlowConfig.string_of_config config);
    Sys_utils.close_out_no_fail file out

  let command = CommandSpec.command spec main
end

module Config = struct
  let spec = {
    CommandSpec.
    name = "config";
    doc = "Read from or write to the .flowconfig";
    usage = Printf.sprintf
      "Usage: %s config [COMMAND] \n"
        CommandUtils.exe_name;
    args = CommandSpec.ArgSpec.(
      empty
      |> flag "--get" (optional string)
          ~doc:"Get the value for a given key"
      |> flag "--add" (optional two_tuple_of_strings)
          ~doc:"Adds a new line to the config without altering any \
                existing values"
      |> CommandUtils.from_flag
      |> CommandUtils.root_flag
      |> CommandUtils.json_flags
    )
  }

  let do_get ~json flowconfig key =
    output_string
      stdout
      (FlowConfig.string_of_config ~json ~key flowconfig)

  let do_get_all ~json flowconfig =
    output_string
      stdout
      (FlowConfig.string_of_config ~json flowconfig)

  let do_add ~json flowconfig (key, value) =
    flowconfig
    |> FlowConfig.add ~key ~value
    |> FlowConfig.string_of_config ~json
    |> output_string stdout

  let main get add from root json () =
    FlowEventLogger.set_from from;
    let root = CommandUtils.guess_root root in
    let flowconfig = FlowConfig.get (Server_files_js.config_file root) in

    match get with
    | Some key -> do_get ~json flowconfig key
    | None -> 
        begin match add with
        | Some (key, value) -> do_add ~json flowconfig (key, value)
        | None -> do_get_all ~json flowconfig
        end

  let command = CommandSpec.command spec main
end
