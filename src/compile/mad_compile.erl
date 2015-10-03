-module(mad_compile).
-copyright('Sina Samavati').
-compile(export_all).

compile(Params) ->
    { Cwd, ConfigFile, Conf } = mad_utils:configs(),
    Res = case Params of
         [] -> mad_compile:'compile-deps'(Cwd, ConfigFile, Conf);
         __ -> mad_compile:deps(Cwd, Conf, ConfigFile, [Params])
    end,
    case bool(Res) of
         true -> {error,Params};
         false -> mad_compile:'compile-apps'(Cwd, ConfigFile, Conf) end.

deps(_, _, _, []) -> {ok,deps};
deps(Cwd, Conf, ConfigFile, [H|T]) ->
    {Name, _} = mad_utils:name_and_repo(H),
    Res = case get(Name) == compiled of
          true -> {ok,[]};
          _    -> dep(Cwd, Conf, ConfigFile, Name) end,
    case bool(Res) of
         true  -> {error,Name};
         false -> deps(Cwd, Conf, ConfigFile, T) end.

bool(true) -> true;
bool(false) -> false;
bool({ok,_}) -> false;
bool({error,_}) -> true.

dep(Cwd, _Conf, ConfigFile, Name) ->

    DepsDir = filename:join([mad_utils:get_value(deps_dir, _Conf, ["deps"])]),
    DepPath = filename:join([Cwd, DepsDir, Name]),

    DepConfigFile = filename:join(DepPath, ConfigFile),
    Conf = mad_utils:consult(DepConfigFile),
    Conf1 = mad_script:script(DepConfigFile, Conf, Name),
    Deps = mad_utils:get_value(deps, Conf1, []),
    DepsRes = bool(deps(Cwd, Conf, ConfigFile, Deps)),

    %mad_hooks:apply_hooks(pre_hooks, Conf, Cwd, Name),
    IsNaga = mad_naga:is_naga(Name, Conf1),
    case IsNaga of false -> mad:info("==> ~p~n",[Name]); 
        true -> mad:info("==> ~p IsNaga true~n",[Name]) end,
  
    case IsNaga of false ->
            SrcDir = filename:join([mad_utils:src(DepPath)]),
            AllFiles = files(SrcDir,".yrl$") ++ 
                files(SrcDir,".xrl$") ++ 
                files(SrcDir,".erl$") ++ % comment this to build with erlc/1
                files(SrcDir,".app.src$"),
            Files = case mad_utils:get_value(erl_first_files, Conf1, []) of
                        []         -> AllFiles;
                        FirstFiles ->
                            FirstFiles1 = lists:map(fun ("src/"++F) -> F1=case filename:extension(F) of ".erl" -> F; _->F++".erl"end,filename:join(SrcDir, F1);
                                            (F)->F1 =case filename:extension(F) of ".erl" -> F; _->F++".erl"end, 
                                                     filename:join(SrcDir, F1) end, FirstFiles),
                            FirstFiles1 ++ lists:filter(fun (F) -> lists:member(F, FirstFiles) == false end, AllFiles)
                    end,

            case Files of
                [] -> {ok,Name};
                Files ->
                    IncDir = mad_utils:include(DepPath),
                    EbinDir = mad_utils:ebin(DepPath),
                    LibDirs = mad_utils:get_value(lib_dirs, Conf, []),
                    Includes = lists:flatten([
                                              [{i,filename:join([DepPath,L,D,include])} || D<-mad_utils:raw_deps(Deps) ] % for -include
                                              ++ [{i,filename:join([DepPath,L])}] || L <- LibDirs ]), % for -include_lib
                                                %mad:info("DepPath ~p~n Includes: ~p~nLibDirs: ~p~n",[DepPath,Includes,LibDirs]),

                                                % create EbinDir and add it to code path
                    file:make_dir(EbinDir),
                    code:replace_path(Name,EbinDir),

                    Opts = mad_utils:get_value(erl_opts, Conf1, []),
                    FilesStatus = compile_files(sorted_files(Files),IncDir, EbinDir, Opts,Includes),
                    DTLStatus = mad_dtl:compile(DepPath,Conf1),
                    PortStatus = lists:any(fun(X)->X end,mad_port:compile(DepPath,Conf1)),

                    put(Name, compiled),
                    case DepsRes orelse FilesStatus orelse DTLStatus orelse PortStatus of
                        true -> {error,Name};
                        false -> {ok,Name} end end;
        true ->
            IncDir = mad_utils:include(DepPath),
            EbinDir = mad_utils:ebin(DepPath),
            LibDirs = mad_utils:get_value(lib_dirs, Conf, []),
            Includes = lists:flatten([
                                      [{i,filename:join([DepPath,L,D,include])} || D<-mad_utils:raw_deps(Deps) ] % for -include
                                      ++ [{i,filename:join([DepPath,L])}] || L <- LibDirs ]), % for -include_lib

            mad_dtl:compile(Name,Conf1),
            case mad_naga:compile(Name, IncDir, EbinDir, Conf1, Includes) of 
                false -> put(Name, compiled), false;
                Err ->
                    io:format("Err ~p~n",[Err]),
                    true
            end
    end.

compile_files([],_,_,_,_) -> false;
compile_files([File|Files],Inc,Bin,Opt,Deps) ->
    case (module(filetype(File))):compile(File,Inc,Bin,Opt,Deps) of
         true -> true;
         false -> compile_files(Files,Inc,Bin,Opt,Deps);
         X -> mad:info("Compilation Error: ~p~n",[{X,File}]), true end.

module("erl")      -> mad_erl;
module("erl.src")  -> mad_utils;
module("yrl")      -> mad_yecc;
module("xrl")      -> mad_leex;
module("app.src")  -> mad_app;
module(_)          -> mad_none.

filetype(Path) -> string:join(tl(string:tokens(filename:basename(Path), ".")), ".").
files(Dir,Ext) -> filelib:fold_files(Dir, Ext, true, fun(F, Acc) -> [F|Acc] end, []).
is_compiled(BeamFile, File) -> mad_utils:last_modified(BeamFile) >= mad_utils:last_modified(File).

'compile-apps'(Cwd, ConfigFile, Conf) ->
    Dirs = mad_utils:sub_dirs(Cwd, ConfigFile, Conf),
    [put(D,0)||D<-Dirs],
    case Dirs of
           [] -> mad_compile:dep(Cwd,  Conf, ConfigFile, Cwd);
         Apps -> mad_compile:dep(Cwd,  Conf, ConfigFile, Cwd),
                 mad_compile:deps(Cwd, Conf, ConfigFile, Apps) end.

'compile-deps'(Cwd, ConfigFile, Conf) ->
    SortedDeps = mad_lock:ordered_deps(Conf, Cwd),
    %mad:info("Sorted Deps ~p~n",[SortedDeps]),
    mad_compile:deps(Cwd, Conf, ConfigFile, SortedDeps).

list(X) when is_atom(X) -> atom_to_list(X);
list(X) -> X.

%% for boss app
sorted_files(Files) ->
    G = digraph:new(),
    [ digraph:add_vertex(G, N) || N <- Files ],
    case all_edges(Files) of
        [] -> Files;
        Edges ->
            [digraph:add_edge(G, A, B) || {A,B} <- Edges],
            lists:reverse(digraph_utils:topsort(G))
    end.

all_edges(Files) ->
    Tmp = [{filename:basename(F), F} || F <- Files],
    all_edges(Files, Tmp, []).

all_edges([], _, Acc) -> Acc;
all_edges([H|T], Files, Acc) ->
    {ok, Fd} = file:open(H, [read]),
    Edges = parse_attrs(H, Fd, Files, []),
    all_edges(T, Files, Edges ++ Acc).

parse_attrs(File, Fd, Files, Includes) ->
    case io:parse_erl_form(Fd, "") of
        {ok, Form, _Line} ->
            case erl_syntax:type(Form) of
                attribute ->
                    NewIncludes = process_attr(File, Form, Files, Includes),
                    parse_attrs(File, Fd, Files, NewIncludes);
                _ ->
                    parse_attrs(File, Fd, Files, Includes)
            end;
        {eof, _} ->
            Includes;
        _Err ->
            parse_attrs(File, Fd, Files, Includes)
    end.

process_attr(File, Form, Files, Includes) ->
    AttrName = erl_syntax:atom_value(erl_syntax:attribute_name(Form)),
    process_attr(File, Form, Files, Includes, AttrName).

process_attr(File, Form, Files, Includes, behaviour) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    case erl_syntax:atom_value(FileNode) of
        application -> Includes;
        gen_event -> Includes;
        gen_server -> Includes;
        supervisor -> Includes;
        Mod -> 
            edge(File, Mod, Files, Includes) end;
process_attr(File, Form, Files, Includes, compile) ->
    [Arg] = erl_syntax:attribute_arguments(Form),
    case erl_syntax:concrete(Arg) of
        {parse_transform, Mod} ->
            edge(File, Mod, Files, Includes);
        {core_transform, Mod} ->
            edge(File, Mod, Files, Includes);
        _ ->
            Includes
    end;
process_attr(_File, _Form, _Files, Includes, _AttrName) ->
    Includes.

edge(File, Mod, Files, Includes) ->
    F = atom_to_list(Mod) ++ ".erl",
    case proplists:get_value(F, Files) of
        undefined -> Includes;
        Else ->             
            [{File, Else}|Includes]
    end.
             
