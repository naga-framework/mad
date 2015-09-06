-module(mad_release).
-compile(export_all).

release(Name,Apps) ->
    file:write_file("relx.config",list_to_binary(io_lib:format(
      "{release,{~s,\"1.0.0\"},~p}.\n"
      "{include_erts,true}.\n"
      "{extended_start_script,true}.\n"
      "{generate_start_script,true}.\n"
      "{vm_args,\"vm.args\"}.\n"
      "{sys_config,\"sys.config\"}.\n"
      "{overlay,[{mkdir,\"log/sasl\"}]}.\n",[Name,Apps]))).

main([]) -> main(["sample"]);
main(Params) ->
    [N|_] = Params,
    {ok,Apps} = mad_plan:orderapps(), %[ filename:basename(F) || F <- filelib:wildcard("{apps,deps}/*"),  filelib:is_dir(F)],
    release(N,Apps),
    {_,Status,X} = sh:run("relx",[],binary,".",[]),
    case Status == 0 of
         true -> false;
         false -> mad:info("Shell Error: ~s~n",[binary_to_list(X)]), true end.
