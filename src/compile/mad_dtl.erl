-module(mad_dtl).
-copyright('Sina Samavati').
-compile(export_all).

compile(Dir,Config) ->
    case mad_utils:get_value(erlydtl_opts, Config, []) of
        [] -> false;
         X -> compile_erlydtl_files(validate_erlydtl_opts(Dir,X)) end.

get_kv(K, Opts, Default) ->
    V = mad_utils:get_value(K, Opts, Default),
    KV = {K, V},
    {KV, Opts -- [KV]}.

file_to_beam(Bin, Filename) -> filename:join(Bin, filename:basename(Filename) ++ ".beam").

validate_erlydtl_opts(Cwd, Opts) ->
    DefaultDocRoot = filename:join("priv", "templates"),
    {DocRoot, Opts1} = get_kv(doc_root, Opts, DefaultDocRoot),
    {OutDir, Opts2} = get_kv(out_dir, Opts1, "ebin"),
    {CompilerOpts, Opts3} = get_kv(compiler_options, Opts2, []),
    {SourceExt, Opts4} = get_kv(source_ext, Opts3, ".dtl"),
    {ModuleExt, Opts5} = get_kv(module_ext, Opts4, ""),

    {_, DocRootDir} = DocRoot,
    DocRoot1 = {doc_root, filename:join(Cwd, DocRootDir)},
    {_, OutDir1} = OutDir,
    OutDir2 = {out_dir, filename:join(Cwd, OutDir1)},

    [DocRoot1, OutDir2, CompilerOpts, SourceExt, ModuleExt|Opts5].

module_name(false, File, [{Ext,NewExt}|_]=ViewsOpts) ->
    list_to_atom(filename:basename(File, Ext) ++ NewExt);
module_name(true, File, [{Ext,NewExt}|_]=ViewsOpts) ->
    Name = mad_naga:view_module(filename:split((File -- mad_utils:cwd()) -- "/"),ViewsOpts),
    list_to_atom(Name).

compile_erlydtl_files(Opts) ->
    {ok, Cwd} = file:get_cwd(),

    {{_, DocRoot},   Opts1} = get_kv(doc_root,   Opts,  ""),
    {{_, SourceExt}, Opts2} = get_kv(source_ext, Opts1, ""),
    {{_, ModuleExt}, Opts3} = get_kv(module_ext, Opts2, ""),
    {{_, OutDir},    Opts4} = get_kv(out_dir,    Opts3, ""),
    {{_, Views },    Opts5} = get_kv(naga,       Opts4, []),
    {{_, Force },        _} = get_kv(force,      Opts5, false), 
 
    ViewsOpts = [{SourceExt,ModuleExt}]++Views,

    Files = lists:foldl(fun({Ext,_},Bcc) -> 
                            B = filelib:fold_files(DocRoot, Ext, true, fun(F, Acc) -> [F|Acc] end, []),
                            B++Bcc end, [], ViewsOpts),

    Compile = fun(F) ->
        ModuleName = module_name(Views =/= [], F, ViewsOpts),
        BeamFile = file_to_beam(OutDir, atom_to_list(ModuleName)),
        Compiled = mad_compile:is_compiled(BeamFile, F),
        %%case Compiled of false ->
        if  Compiled =:= false orelse Force ->
             mad:info("DTL Compiling ~s -> ~s~n", [F -- mad_utils:cwd(), ModuleName]),
             Res = erlydtl:compile(F, ModuleName, Opts3),
             file:change_time(BeamFile, calendar:local_time()),
             case Res of {error,Error} -> mad:info("Error: ~p~n",[Error]);
                                    OK -> OK end;
             true -> ok end
    end,

    lists:any(fun({error,_}) -> true; (ok) -> false end,[Compile(F) || F <- Files]).
