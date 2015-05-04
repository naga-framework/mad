-module(mad_compile).
-copyright('Sina Samavati').
-compile(export_all).

%% compile dependencies

deps(_, _, _, []) -> false;
deps(Cwd, Conf, ConfigFile, [{Name,Path}|T]) ->
    Res = case get(Name) == compiled andalso get(mode) /= active  of
              true -> false;
              false -> dep(Cwd, Conf, ConfigFile, Name, Path) end,
    case Res of 
        true -> true;
        false -> deps(Cwd, Conf, ConfigFile, T) end.

%% compile a dependency
dep(Cwd, Conf, ConfigFile, "/"++ _ = Name) ->
    dep(Cwd, Conf, ConfigFile, filename:basename(Name), Cwd).
%%FIXME
dep(_Cwd, _Conf, _ConfigFile, "merl"=Name, DepPath) -> 
    EbinDir = mad_utils:ebin(DepPath),    
    code:replace_path(Name,EbinDir),
    put(Name, compiled),
    false;
dep(Cwd, _Conf, ConfigFileName, Name, Path) ->
    ConfigFile = filename:join(Path, ConfigFileName),
    Conf = mad_utils:consult(ConfigFile),

    io:format("==> ~p~n\r",[Name]),

    Conf1 = mad_script:script(ConfigFile, Conf, Name),
    Deps = mad_utils:get_value(deps, Conf1, []),
    SrcDir = filename:join([mad_utils:src(Path)]),
    mad_hooks:apply_hooks(pre_hooks, Conf, Cwd, Path),

    case mad_naga:is_naga(Name, Conf) of false ->
    Files = files(SrcDir,".yrl") ++ 
            files(SrcDir,".xrl") ++ 
            files(SrcDir,".lfe") ++ 
            files(SrcDir,".erl") ++ % comment this to build with erlc/1
            files(SrcDir,".app.src"),

    case Files of
        [] -> false;
        Files ->
            IncDir = mad_utils:include(Path),
            EbinDir = mad_utils:ebin(Path),
            LibDirs = mad_utils:get_value(lib_dirs, Conf, []),
            Includes = lists:flatten([
               [{i,filename:join([filename:dirname(Path),D,include])} 
                || D<-mad_utils:raw_deps(Deps) ] %% for -include
            ++ [{i,filename:dirname(Path)}] ]
            ++ [{i,filename:join([Cwd,L])} || L <- LibDirs ]), % for -include_lib
            %io:format("DepPath ~p~n Includes: ~p~nLibDirs: ~p~n",[DepPath,Includes,LibDirs]),
            %% create EbinDir and add it to code path
            file:make_dir(EbinDir),
            code:replace_path(Name,EbinDir),

            %erlc(DepPath), % comment this to build with files/2

            Opts = mad_utils:get_value(erl_opts, Conf1, []),
            FilesStatus = compile_files(sorted_files(Files),IncDir, EbinDir, Opts, Includes),
            DTLStatus = mad_dtl:compile(Path,Conf1),
            PortStatus = lists:any(fun(X)->X end,mad_port:compile(Path,Conf1)),
            %% io:format("DTL Status: ~p~n",[DTLStatus]),
            %% io:format("Port Status: ~p~n",[PortStatus]),
            %% io:format("Files Status: ~p~n",[FilesStatus]),
            put(Name, compiled),
            FilesStatus orelse DTLStatus orelse PortStatus
    end;
        true ->
            EbinDir = mad_utils:ebin(Path),
            file:make_dir(EbinDir),
            code:replace_path(Name,EbinDir),
            mad_dtl:compile(Path,Conf1),
            mad_naga:compile(Path,Conf1),
            put(Name, compiled),
            false %%FIXME
    end.

compile_files([],_Inc,_Bin,_Opt,_Deps) -> false;
compile_files([File|Files],Inc,Bin,Opt,Deps) ->
    case (module(filetype(File))):compile(File,Inc,Bin,Opt,Deps) of
         true -> true;
         false -> compile_files(Files,Inc,Bin,Opt,Deps);
         _ -> io:format("Error: ~p~n",[{File}]) end.

module("erl") -> mad_erl;
module("lfe") -> mad_lfe;
module("erl.src") -> mad_utils;
module("yrl") -> mad_yecc;
module("xrl") -> mad_leex;
module("app.src") -> mad_app;
module(_) -> mad_none.

filetype(Path) -> string:join(tl(string:tokens(filename:basename(Path), ".")), ".").
files(Dir,Ext) -> filelib:fold_files(Dir, Ext, true, fun(F, Acc) -> [F|Acc] end, []).
is_compiled(BeamFile, File) -> mad_utils:last_modified(BeamFile) >= mad_utils:last_modified(File).

'compile-apps'(Cwd, ConfigFile, Conf) ->
    Dirs = mad_utils:sub_dirs(Cwd, ConfigFile, Conf),
    %io:format("Compile Apps: ~p~n",[Dirs]),
    case Dirs of
           [] -> mad_compile:dep(Cwd,  Conf, ConfigFile, Cwd);
         Apps -> mad_compile:dep(Cwd,  Conf, ConfigFile, Cwd),
                 mad_compile:deps(Cwd, Conf, ConfigFile, Apps) end.

'compile-deps'(Cwd, ConfigFile, Conf) ->
    SortedDeps = mad_lock:ordered_deps(Conf, Cwd),    
    mad_compile:deps(Cwd, Conf, ConfigFile, SortedDeps).

list(X) when is_atom(X) -> atom_to_list(X);
list(X) -> X.

erlc(DepPath) ->
    ErlFiles = filelib:wildcard(DepPath++"/src/**/*.erl"),
    io:format("Files: ~s~n\r",[[filename:basename(Erl)++" " ||Erl<-ErlFiles]]),
    {_,Status,X} = sh:run("erlc",["-o"++DepPath++"/ebin/","-I"++DepPath++"/include"]++
        ErlFiles,binary,filename:absname("."),[{"ERL_LIBS","apps:deps"}]),
    case Status == 0 of
         true -> skip;
         false -> io:format("Error: ~s~n\r",[binary_to_list(X)]) end.


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
             
    



