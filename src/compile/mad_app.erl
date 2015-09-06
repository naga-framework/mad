-module(mad_app).
-copyright('Sina Samavati').
-compile(export_all).

app_src_to_app(Filename) -> filename:basename(Filename, ".app.src") ++ ".app".

to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) -> X.

validate_property({modules, _}, Modules) -> {modules, Modules};
validate_property({vsn, Value}, _) -> {vsn, to_list(Value)};
validate_property(Else, _) -> Else.

compile(File,_Inc,Bin,_Opt,_Deps) ->
    AppFile = filename:join(Bin, app_src_to_app(File)),
    Compiled = mad_compile:is_compiled(AppFile, File),
    if  Compiled =:= false ->
<<<<<<< HEAD
    io:format("Writing ~s~n\r", [AppFile -- mad_utils:cwd()]),
    BeamFiles = filelib:wildcard("*.beam", Bin),
    Modules = [list_to_atom(filename:basename(X, ".beam")) || X <- BeamFiles],
    [Struct|_] = mad_utils:consult(File),
    {application, AppName, Props} = Struct,
    Props1 = add_modules_property(Props),
    Props2 = [validate_property(X, Modules) || X <- Props1],
    Struct1 = {application, AppName, Props2},
    file:write_file(AppFile, io_lib:format("~p.~n", [Struct1])),
    false;
    true -> false end.
=======
        io:format("Writing ~s~n\r", [AppFile -- mad_utils:cwd()]),
        BeamFiles = filelib:wildcard("*.beam", Bin),
        Modules = [list_to_atom(filename:basename(X, ".beam")) || X <- BeamFiles],
        [Struct|_] = mad_utils:consult(File),
        {application, AppName, Props} = Struct,
        Props0 = add_modules_property(Props),
        Props1 = generate_deps(Props0),
        Props2 = [validate_property(X, Modules) || X <- Props1],
        Struct1 = {application, AppName, Props2},
        file:write_file(AppFile, io_lib:format("~p.~n", [Struct1])),
        false;
        true -> false end.
>>>>>>> 472d7e6... generate applications if not set

add_modules_property(Properties) ->
    case lists:keyfind(modules, 1, Properties) of
        {modules, _} -> Properties;
        _ -> Properties ++ [{modules, []}] end.

<<<<<<< HEAD
=======
generate_deps(Properties) ->
    case lists:keyfind(applications, 1, Properties) of
         false -> Properties ++ [apps()];
        Exists -> Properties ++ [Exists] end.

apps() -> {ok,Apps} = mad_plan:orderapps(),
          {applications,Apps}.
>>>>>>> 472d7e6... generate applications if not set
