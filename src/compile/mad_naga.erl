-module(mad_naga).
-copyright('chan sisowath').
-compile(export_all).

-define(COMPILE_OPTS(Inc, Ebin, Opts, Deps), [report, {i, [Inc]}, {outdir, Ebin}] ++ Opts ++ Deps).

help() ->
    io:format("              dtl strings <path/to/tpl>~n"),
    io:format("              naga files <path/2/app> [view|controller|websocket|mail|mail_view|rest|lib]~n"),
    io:format("              naga modules <path/2/app> [view|controller|websocket|rest]~n"),
    io:format("              naga create name=<appname>~n").

naga_default_opts() ->
    [
     {model_manager,     none} %none | boss
    ,{controller_manager,none} %erl|lfe|pmod|boss 
    ,{session_manager,   none} %naga_session
    ,{i18n_manager,      none} %naga_i18n
    ,{mail_manager,      none} %naga_smtp
    ,{static_manager,    cowboy_static} % naga_static     
    ,{src_extension,     [".erl", ".lfe", ".ex"]}
    ,{erl_extension,     [".erl"]}
    ,{lfe_extension,     [".lfe"]}
    ,{elixir_extension,  [".ex"]}
    ,{tpl_extension,     [".html", ".css", ".js", ".txt", ".xml", ".json", ".dtl"]}
    ,{mail_tpl_extension,[".html", ".txt"]}
    ,{include_dir,       "include"}
    ,{ebin_dir,          "ebin"}
    ,{priv_dir,          "priv"}
    ,{static_dir,        "priv/static"}
    ,{download_dir,      "scratch"}
    ,{init_dir,          "priv/init"}
    ,{lang_dir,          "priv/lang"}
    ,{fcgi_dir,          "priv/fcgi"}
    ,{migration_dir,     "priv/migrations"}     
    ,{src_dir,           "src"}
    ,{controller_dir,    "src/controller"}
    ,{n2o_dir,           "src/n2o"}     %"src/n2o/handler"
    ,{lib_dir,           "src/lib"}
    ,{mail_dir,          "src/mail"}
    ,{websocket_dir,     "src/websocket"}
    ,{model_dir,         "src/model"}
    ,{rest_dir,          "src/rest"}
    ,{wamp_dir,          "src/wamp"}
    ,{mail_view_dir,     "src/mail/view"}
    ,{view_dir,          "src/view"}
    ,{view_lib_dir,      "src/view/lib"}
    ,{view_tag_dir,      "src/view/lib/tag_modules"}
    ,{view_filter_dir,   "src/view/lib/filter_modules"}
    ,{view_htmltags_dir, "src/view/lib/tag_html"}
    ,{custom_tags,       "priv/custom_tags"}

    ,{src,               [lib, model, view_tag_helper, view_filter_helper, mail, websocket, n2o_dtl,
                           view_html_tags, view, mail_view, controller, rest, n2o, src]}
    ].

%% --------------------------------------------------------------------------------------
%% compile NAGA App
%% --------------------------------------------------------------------------------------
compile(Cwd, Inc, Bin, Config, Deps, true) ->
    compile(Cwd, Inc, Bin, Config, Deps);
compile(Path, Inc, Bin, Config, Deps, false) ->
    case filelib:is_file(Path) of
        true ->
            Ext = filename:extension(Path),
            {Bin, Inc} = case filename:split(Path) of
                             ["..", App, "src"|_] -> 
                                 {filename:join(["..",App, "ebin"]),
                                  filename:join(["..",App, "include"])};
                             ["..", App, "priv"|_] -> 
                                 {filename:join(["..",App, "ebin"]),
                                  filename:join(["..",App, "include"])};
                             ["deps", App, "src"|_] -> 
                                 {filename:join(["deps",App, "ebin"]),
                                  filename:join(["deps",App, "include"])};
                             ["apps", App, "src"|_] -> 
                                 {filename:join(["apps",App, "ebin"]),
                                  filename:join(["apps",App, "include"])};
                             ["src"|_] -> 
                                 {"./ebin", "./include" }                             
                         end,
            %%io:format("Bin ~p, Inc ~p~n, Ext ~p",[Bin, Inc, Extension]),
            case lists:member(Ext, [".erl", ".lfe"]) of true ->
            case compile_files([Path], Inc, Bin, Config, Deps, []) of
                {false, _} -> false;
                _ -> true
            end; false -> true end;

        false ->
            case filelib:is_dir(Path) of true ->
                    mad:main(["compile", Path]);
                false -> true
            end
    end.

compile(Cwd, Inc, Bin, Config, Deps) ->
    {{_, NagaOpts}, C1} = get_kv(naga_opts, Config, []),
    {{_,ErlOpts}, _} = get_kv(erl_opts, C1, []),
    case lists:keydelete(name, 1, NagaOpts) of
        [] -> compile_naga(Cwd, Inc, Bin, naga_default_opts()++[{erl_opts, ErlOpts}], Deps);           
        X -> compile_naga(Cwd, Inc, Bin, overide(naga_default_opts(),X)++[{erl_opts, ErlOpts}], Deps) 
    end.

%% --------------------------------------------------------------------------------------
%% compile NAGA App
%% --------------------------------------------------------------------------------------
compile_naga(Path, Inc, Bin, Opts0, Deps) ->    
    case filelib:is_dir(Path) of
        true ->
            Opts = [{root_dir, Path}|Opts0],
            code:replace_path(list_to_atom(app_name(Path)), out_dir(Opts)),                        

            SrcDir = dir(src_dir, Opts),
            InitDir = dir(init_dir, Opts),
            SrcExtensions = proplists:get_value(src_extension, Opts, []),
            Extensions = SrcExtensions ++ proplists:get_value(tpl_extension, Opts) ++
                proplists:get_value(mail_tpl_extension, Opts),
            Exts = lists:usort(Extensions),
            Files0 = files_list(SrcDir, Exts, []) ++ files_list(InitDir, SrcExtensions, []),
            {ok, Cwd} = file:get_cwd(),
            Files = [begin
                         Temp = (X -- Cwd) -- "/",
                          case Path of
                              "/" ++ _ -> 
                                  case filename:split(Temp) of
                                      ["deps", _Name | _] ->
                                          Temp;
                                      _ ->
                                          AppName = filename:basename(Path),
                                          filename:join(["..", AppName, Temp])
                                  end;
                              _ -> Temp
                          end
                      end || X <- Files0],
            case compile_files(Files, Inc, Bin, Opts, Deps, []) of
                {false, Modules} -> 
                    emit_app_src(Modules, Path, Opts), 
                    false;
                _ -> 
                    true
            end;
        false -> 
            {File, Opts2} = case root_dir(Path) of
                                 "/" -> 
                                     {ok, Cwd} = file:get_cwd(),                                     
                                     Temp = (Path -- Cwd) -- "/",
                                     AppName = filename:basename(Cwd),
                                     RootDir = filename:join(["..", AppName]),
                                     RelPath = filename:join([RootDir, Temp]),
                                     Opts1 = [{root_dir, RootDir}|Opts0],
                                     {RelPath, Opts1};
                                 _ ->
                                     Opts1 = [{root_dir, root_dir(Path)}|Opts0],
                                     {Path, Opts1}
                             end,
            case compile_files([File], Inc, Bin, Opts2, Deps, []) of
                {false, _} -> false;
                _ -> true
            end
    end.


compile_files([], _Inc, _Bin, _Opts, _Deps, Acc) -> {false, Acc};
compile_files([File|Files], Inc, Bin, Opts, Deps, Acc) ->
    {App, Type} = t(File),
    CompileFun = h(Type),
    Res = case Type of
              view -> ?MODULE:CompileFun(App, File, Inc, Bin, Opts, Deps);
              _ -> ?MODULE:CompileFun(File, Inc, Bin, Opts, Deps)
          end,
    
    case Res of
        {ok, {compiled,Module}} ->
          mad:info("Compiling ~p ~s~n", [Type, File]), 
          compile_files(Files, Inc, Bin, Opts, Deps, [Module|Acc]);
        {ok, Module} ->
          %mad:info("Compiling ~p ~s~n", [Type, File]), 
          compile_files(Files, Inc, Bin, Opts, Deps, [Module|Acc]);
        Err ->
            {true, Acc}
    end.

skip(_,_,_,_,_) -> {true, []}.     
%% --------------------------------------------------------------------------------------
%%  compile erlang
%% --------------------------------------------------------------------------------------
compile_erl(File, Inc,  Bin, Opts, Deps) ->
    Opt0 = proplists:get_value(erl_opts, Opts, []),
    BeamFile = erl_to_beam(Bin, File),
    Compiled = mad_compile:is_compiled(BeamFile, File),
    Module = list_to_atom(filename:rootname(filename:basename(BeamFile))),
    %% mad:info("Bin ~p~n, Inc ~p~n, Deps ~p~n, Module ~p~n, Opts ~p~n",
    %%           [Bin, Inc, Deps, Module, Opt0]),
    if  Compiled =:= false ->
        Opts1 = ?COMPILE_OPTS(Inc, Bin, [verbose] ++ Opt0, Deps),
        case compile:file(File, Opts1) of
            ok -> {ok, {compiled,Module}};
            Err -> Err end;
        true -> {ok, Module} end.

%FIXME include dir ?, parse_transform opts in erl_opts ??
%% --------------------------------------------------------------------------------------
%%  compile lfe
%% --------------------------------------------------------------------------------------

compile_lfe(File, Inc, Bin, Opts, Deps) ->
    Opts0 = [verbose, return, binary, {parse_transform, pmod_pt}] ++ 
        proplists:get_value(erl_opts, Opts, []),
    BeamFile = erl_to_beam(Bin, File),
    Compiled = mad_compile:is_compiled(BeamFile, File),
    Module = list_to_atom(filename:rootname(filename:basename(BeamFile))),
    if  Compiled =:= false ->
            Opts1 = ?COMPILE_OPTS(Inc, Bin, [verbose] ++ Opts0, Deps),            
            case lfe_comp:file(File, Opts1) of
                {ok, Module, Binary, _Warnings} ->
                    OutFile = filename:join([Bin, filename:basename(File, ".lfe") ++ ".beam"]),
                    file:write_file(OutFile, Binary),            
                    {ok, {compiled,Module}};
                Other -> Other end;
        true ->  {ok, Module} end.

   
%% --------------------------------------------------------------------------------------
%%  compile model
%% --------------------------------------------------------------------------------------
compile_model(File, Inc, Bin, Opts, Deps) ->
    compile_model(proplists:get_value(model_manager,Opts), File, Inc, Bin, Opts, Deps).
compile_model(none, File, Inc, Bin, Opts, Deps) -> 
    compile_model_none(filename:extension(File), File, Inc, Bin, Opts, Deps);
compile_model(boss, File, Inc, Bin, Opts, Deps) ->
    Compiler = boss_record_compiler,
    ErlOpts = proplists:get_value(erl_opts, Opts, []),
    Opts1 = [debug_info,
             {pre_revert_transform, fun Compiler:trick_out_forms/2},
             {token_transform,      fun Compiler:process_tokens/1}
             , {i, Inc}, ErlOpts ++ Deps, {out_dir, Bin}] ,
    Compiler:compile(File, Opts1);

compile_model(_, File, Inc, Bin, Opts, Deps) -> 
    compile_model(none, File, Inc, Bin, Opts, Deps).

compile_model_none(".lfe", File, Inc, Bin, Opts, Deps) -> compile_lfe(File, Inc, Bin, Opts, Deps);
compile_model_none(".erl", File, Inc, Bin, Opts, Deps) -> compile_erl(File, Inc, Bin, Opts, Deps);
compile_model_none(    _,    _,   _,   _,    _,    _) -> skip.

%% --------------------------------------------------------------------------------------
%%  compile controller
%% --------------------------------------------------------------------------------------
compile_controller(File, Inc, Bin, Opts, Deps) ->
    compile_controller(filename:extension(File), File, Inc, Bin, Opts, Deps).

compile_controller(".erl", File, Inc, Bin, Opts, Deps) -> 
    case proplists:get_value(controller_manager, Opts) of
        boss -> boss_legacy:compile(File, Opts);
        _ -> compile_erl(File, Inc, Bin, Opts, Deps) end;
compile_controller(".lfe", File, Inc, Bin, Opts, Deps) -> 
    compile_lfe(File, Inc, Bin, Opts, Deps).

%% --------------------------------------------------------------------------------------
%%  compile view template
%% --------------------------------------------------------------------------------------

compile_view(AppName, File, _Inc, _Bin, Opts, _Deps) ->    
    Dir = case proplists:get_value(root_dir, Opts) of
              undefined -> case filename:split(File) of
                               ["deps", AppName |_] -> filename:join([".", "deps", AppName]);
                               [".","src"|_] -> {ok,Cwd} = file:get_cwd(), Cwd
                           end;
              Else -> Else end,
    compile_view(AppName, Dir, File, Opts).
    
compile_view(AppName, Dir, File, Opts) ->
    Tokens = case lists:prefix(Dir, File) of 
                 true  ->filename:split(File) -- filename:split(Dir);
                 false ->filename:split(File)
             end,
    ViewModule = view_module(AppName, Tokens),
    compile_view(AppName, Dir, File, ViewModule, Opts).    

compile_view(_AppName, Dir, File, Module, Opts) ->
    OutDir = dir(ebin_dir, Opts, Dir),
    DocRoot = dir(view_dir, Opts, Dir),
    TplOpts = overide(tpl_opts(Opts), [{out_dir,OutDir},{doc_root, DocRoot}]),
    BeamFile = file_to_beam(OutDir, atom_to_list(Module)),
    Compiled = mad_compile:is_compiled(BeamFile, File),
    if  Compiled =:= false ->
           TplOpts1 = [{compiler_options,[verbose]}, report | TplOpts],
           case erlydtl:compile_file(File, Module, TplOpts1) of
            {ok, M} -> {ok, {compiled,M}}; Err -> Err end;
        true -> {ok, Module}
    end.

view_module(App, Path) ->
    naga_module(App, Path).

%% --------------------------------------------------------------------------------------
%%  compile rest controller
%% --------------------------------------------------------------------------------------
compile_rest(File, Inc, Bin, Opts, Deps) ->
    Dir = proplists:get_value(root_dir, Opts),
    AppName = app_name(Dir),
    File1 = filename:rootname(File),
    Tokens = case lists:prefix(Dir, File1) of 
                 true  ->filename:split(File1) -- filename:split(Dir);
                 false ->filename:split(File1)
             end,
    Module = rest_module(AppName, Tokens),
    BeamFile = erl_to_beam(Bin, Module),

    Compiled = mad_compile:is_compiled(BeamFile, File),
    case Compiled of
        false ->
            Opts0 = proplists:get_value(erl_opts, Opts),
            Opts1 = ?COMPILE_OPTS(Inc, Bin, Opts0 ++ 
                                      [{module_name, Module}, verbose, 
                                       return, binary, {parse_transform, ?MODULE}
                                      ] , Deps),     
            case compile:file(File, Opts1) of
                {ok, _ModuleName, Binary, _Warning} ->                     
                    case file:write_file(BeamFile, Binary) of
                        ok -> 
                            {ok, {compiled,Module}};
                        {error, Reason} = Err ->
                            Err
                    end;
                Err -> 
                    mad:info("Error ~p",[Err]),
                    Err end;
        true ->
            {ok, Module}
    end.

rest_module(App, Path) ->
    naga_module(App, Path).


%% --------------------------------------------------------------------------------------
%% 
%% --------------------------------------------------------------------------------------
naga_module(App, Path) when is_atom(App) ->
    naga_module(atom_to_list(App), filename:split(Path));
naga_module(App, Tokens) -> n_m(App, Tokens).
n_m(App,["..",App,"apps", App, "src"| C]) -> n_m(App, C); 
n_m(App,[     "apps", App, "src"| C]) -> n_m(App, C); 
n_m(App,[     "deps", App, "src"| C]) -> n_m(App, C);  
n_m(App,[".", "apps", App, "src"| C]) -> n_m(App, C);  
n_m(App,[".", "deps", App, "src"| C]) -> n_m(App, C);  
n_m(App,[     "deps", App, "src"| C]) -> n_m(App, C);  
n_m(App,[                  "src"| C]) -> n_m(App, C);
n_m(App,[       "..", App, "src"| C]) -> n_m(App, C);
n_m(App,[        ".", App, "src"| C]) -> n_m(App, C);
n_m(App, Components) ->
    %io:format("App:~p, T:~p~n",[App,Components]),
    Lc = string:to_lower(lists:concat([App, "_", string:join(Components, "_")])),
    ModuleIOList = re:replace(Lc, "\\.", "_", [global]),
    list_to_atom(binary_to_list(iolist_to_binary(ModuleIOList))).


parse_transform(Forms, Opts) ->
    transform_module_name(Forms, Opts).

transform_module_name(Forms, Opts) ->
    To = proplists:get_value(module_name, Opts),
    add_module_name(Forms, To).

add_module_name([{attribute,A,module,_}|Fs], Name) ->
    F1 = {attribute, A, module, Name},
    [F1|Fs];
add_module_name([F|Fs], Attrs) ->
    [F|add_module_name(Fs, Attrs)].


%%FIXME 
tpl_opts(Opts) ->        
    M = fun(X,Y) -> modules(X,Y,Opts) end,
    G = fun(X) -> proplists:get_value(X,Opts) end,
    GG= fun(X,Y) -> proplists:get_value(X,Opts,Y) end,
    [{doc_root, dir(view_dir,Opts)}
    ,{auto_escape, GG(auto_escape,true)}
    ,{out_dir, dir(ebin_dir, Opts)}
    ,{custom_filters_modules, M(erl,G(view_tag_dir)) ++ M(erl, G(view_filter_dir))}
    ,{custom_tags_modules, M(erl, G(custom_tags))}
    ,{custom_tags_dir, M(view, G(view_htmltags_dir))}    
    %,{compiler_options, G(compiler_options)}
    ].


t(Path) ->
    case filename:split(Path) of
        [_, App, "apps", App, "src", "controller" |_]                     -> {App, controller};
        [_, App, "apps", App, "src", "view", "lib", "tag_html" | _]       -> {App, view_tag_helper};
        [_, App, "apps", App, "src", "view", "lib", "filter_modules" | _] -> {App, view_filter_helper};
        [_, App, "apps", App, "src", "view", "lib", "tag_modules" | _]    -> {App, view_custom_tags};
        [_, App, "apps", App, "src", "view"|_]                            -> {App, view};
        [_, App, "apps", App, "src", "mail", "view"|_]                    -> {App, mail_view};
        [_, App, "apps", App, "src", "lib"|_]                             -> {App, lib};
        [_, App, "apps", App, "src", "mail"|_]                            -> {App, mail};
        [_, App, "apps", App, "src", "model"|_]                           -> {App, model};
        [_, App, "apps", App, "src", "websocket"|_]                       -> {App, websocket};
        [_, App, "apps", App, "src", "wamp"|_]                            -> {App, wamp};
        [_, App, "apps", App, "src", "rest"|_]                            -> {App, rest};
        [_, App, "apps", App, "src", "n2o"|_]                             -> {App, n2o};
        [_, App, "apps", App, "src"|_] = P ->
            case filename:extension(P) of
                ".lfe" -> {App, lfe};
                ".ex"  -> {App, elixir};
                ".erl" -> {App, erlang}
            end;
        %% [_, App, "src", "controller" |_]                     -> {App, controller};
        %% [_, App, "src", "view", "lib", "tag_html" | _]       -> {App, view_tag_helper};
        %% [_, App, "src", "view", "lib", "filter_modules" | _] -> {App, view_filter_helper};
        %% [_, App, "src", "view", "lib", "tag_modules" | _]    -> {App, view_custom_tags};
        %% [_, App, "src", "view"|_]                            -> {App, view};
        %% [_, App, "src", "mail", "view"|_]                    -> {App, mail_view};
        %% [_, App, "src", "lib"|_]                             -> {App, lib};
        %% [_, App, "src", "mail"|_]                            -> {App, mail};
        %% [_, App, "src", "model"|_]                           -> {App, model};
        %% [_, App, "src", "websocket"|_]                       -> {App, websocket};
        %% [_, App, "src", "wamp"|_]                            -> {App, wamp};
        %% [_, App, "src", "rest"|_]                            -> {App, rest};
        %% [_, App, "src", "n2o"|_]                             -> {App, n2o};
        %% [_, App, "src"|_] = P ->
        %%     case filename:extension(P) of
        %%         ".lfe" -> {App, lfe};
        %%         ".ex"  -> {App, elixir};
        %%         ".erl" -> {App, erlang}
        %%     end;
        %% [_, App, "priv", "init"|_]                           -> {App, init};
         _                                                   -> {undefined, skip}
    end.
                
h(Type) ->
    case Type of
        view               -> compile_view;
        mail_view          -> compile_view;
        controller         -> compile_controller;
        view_filter_helper -> compile_erl;
        mail               -> compile_erl;
        view_tag_helper    -> compile_erl;
        view_custom_tags   -> compile_erl;
        websocket          -> compile_erl; 
        erl                -> compile_erl; 
        erlang             -> compile_erl; 
        elixir             -> compile_elixir; 
        lfe                -> compile_lfe;
        model              -> compile_model; 
        lib                -> compile_erl;
        init               -> compile_erl;
        n2o                -> compile_erl;
        n2o_dtl            -> compile_view; 
        rest               -> compile_rest;
        src                -> compile_naga;
        skip               -> skip
    end.

modules(_, _, []) -> [];
modules(Type, Dir, Opts) when is_list(Type)->                
    modules(list_to_atom(Type), Dir, Opts);
modules(view, Dir, Opts) ->                
    Files = files(view, Dir, Opts),
    App = app_name(Dir),
    [begin 
         E = filename:extension(File),
         case lists:prefix(Dir, File) of 
             true  ->{E,view_module(App, filename:split(File) -- filename:split(Dir))};
             false ->{E,view_module(App, filename:split(File))}
         end
     end || File <- Files];
modules(rest, Dir, Opts) ->                
    Files = files(rest, Dir, Opts),
    App = app_name(Dir),
    [begin 
         E = filename:extension(File),
         %%FIXME: clash with extention lfe|elixir|erl
         F = filename:rootname(File),
         case lists:prefix(Dir, File) of 
             true  ->{E,rest_module(App, filename:split(F) -- filename:split(Dir))};
             false ->{E,rest_module(App, filename:split(F))}
         end
     end || File <- Files];
modules(Type, Dir, Opts) ->
    Files = files(Type, Dir, Opts),
    [{filename:extension(F),list_to_atom(filename:rootname(filename:basename(F)))}||F<-Files].
    
files(Type, Dir, Opts) when is_list(Type)->
    files(list_to_atom(Type), Dir, Opts); 
files(src, Dir, Opts) -> 
    K = list_to_atom("src_dir"),
    Extensions = extension(src_type(src), Opts),
    Path = proplists:get_value(K, Opts),
    SrcRoot = filename:join([Dir, Path]),
    files_dir(SrcRoot, Extensions);
files(Type, Dir, Opts) when is_atom(Type)-> 
    K = list_to_atom(atom_to_list(Type) ++ "_dir"),
    Extensions = extension(src_type(Type), Opts),
    Path = proplists:get_value(K, Opts),
    SrcRoot = filename:join([Dir, Path]),
    files_list(SrcRoot, Extensions, []).

files_dir(Dir, Extensions) ->
    List = file:list_dir(Dir),
    {ok, Cwd} = file:get_cwd(),
    case List of
        {ok, Files} ->
            [begin 
                 (filename:join(Dir, X) -- Cwd) -- "/"
             end
             ||X <- Files, filelib:is_file(filename:join(Dir,X)), 
               lists:member(filename:extension(X), Extensions)];
        _ -> []
    end.

files_list(_, [], Acc) -> lists:reverse(Acc);
files_list(Root, [Ext|T], Acc) -> 
    Files = filelib:fold_files(Root, Ext, true, 
            fun(F, X) -> case filename:extension(F) == Ext of
                         true -> [F|X];false -> X end end,[]),
    files_list(Root, T, Files ++ Acc).     

extension(src,  Opts) -> proplists:get_value(src_extension, Opts);
extension(erl,  Opts) -> proplists:get_value(erl_extension, Opts);
extension(lfe,  Opts) -> proplists:get_value(lfe_extension, Opts);
extension(tpl,  Opts) -> proplists:get_value(tpl_extension, Opts);
extension(mail, Opts) -> proplists:get_value(mail_tpl_extension, Opts);
extension(cljs, Opts) -> proplists:get_value(cljs_extension, Opts);
extension(less, Opts) -> proplists:get_value(less_extension, Opts);
extension(sass, Opts) -> proplists:get_value(sass_extension, Opts).

src_type(Type) ->
    case Type of
        controller         -> src;
        lib                -> src;
        mail               -> src;
        model              -> src;
        mail_view          -> mail;
        websocket          -> src;
        rest               -> src;
        n2o                -> src;
        n2o_dtl            -> tpl;
        view               -> tpl;
        view_html_tags     -> tpl;
        view_tag_helper    -> erl;
        view_filter_helper -> erl;
        cljs               -> cljs;
        less               -> less;
        sass               -> sass;
        init               -> src;
        erl                -> erl;
        src                -> src;
        _                  -> skip
    end.

out_dir(Opts) -> dir(ebin_dir,Opts).
root_dir(Path) ->
    root_dir2(filename:split(Path)).

root_dir2(["..", App | _]) ->
    filename:join(["..", App]);
root_dir2(["deps", App | _]) ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "deps", App]);
root_dir2(Path) ->
    filename:dirname(Path).

dir(Key, Opts)->
    dir(Key, Opts, ".").
dir(Key, Opts, Dft) ->
    Dir = proplists:get_value(root_dir, Opts, Dft),
    filename:join([Dir,proplists:get_value(Key,Opts)]).
    
app_name(Dir) -> filename:basename(Dir).
emit_app_src(Modules, Dir, Opts) ->
    %AllModules = [element(2,X) || X<- Result, element(1,X) == ok],
    OutDir = out_dir(Opts),
    AppName = app_name(Dir),
    App = list_to_atom(AppName),
    DotAppSrc = filename:join([Dir , "src",  string:concat(AppName, ".app.src")]),
    case file:consult(DotAppSrc) of
        {error,enoent} -> io:format("error file ~p not found~n",[DotAppSrc]), 
                          skip;        
        {ok, [{application, App, AppData}]} ->
            AppData1 = lists:keyreplace(modules, 1, AppData, {modules, Modules}),
            DefaultEnv = proplists:get_value(env, AppData1, []),
            AppData2 = lists:keyreplace(env, 1, AppData1, {env, DefaultEnv}),
            IOList = io_lib:format("~p.~n", [{application, App, AppData2}]),
            AppFile = filename:join([OutDir, lists:concat([AppName, ".app"])]),
            %io:format("~nFile ~p~n---- naga app~n~s~n",[AppFile, IOList]),
            file:write_file(AppFile, IOList),
            false
    end.

    
%% LIB
erl_to_beam(Bin, F) -> filename:join(Bin, filename:basename(F, ".erl") ++ ".beam").

%%FIXME: need to test this
is_naga(Path, Conf) ->
    case filelib:is_dir(Path) of
        true -> is_naga2(Path, Conf);
        false -> case filename:split(Path) of
                     ["deps",App|_] -> is_naga2(filename:join(["deps",App]), Conf);
                     ["apps",App|_] -> is_naga2(filename:join([".","deps",App]), Conf);
                     [  "..",App|_] -> is_naga2(filename:join(["..", App]), Conf);
                     [   ".","deps",App|_] -> is_naga2(filename:join([".","deps",App]), Conf);
                     [ "src"|_] -> is_naga2(file:get_cwd(), Conf);
                     Name -> is_naga2(Name, Conf)
                 end
    end.                                                                           
is_naga2(Name, Conf) when is_list(Name)-> 
    is_naga2(list_to_atom(lists:last(filename:split(Name))), Conf);
is_naga2(Name, Conf) when is_atom(Name)-> 
    Name == proplists:get_value(name, proplists:get_value(naga_opts, Conf, []), false).    

    
cmd(Params) ->   
    mad:info("cmd naga: ~p~n",[Params]).

cmd(Cwd, _, Config, Params) ->   
    %mad:info("Compile Static Params: ~p~n",[Params]),
    %mad:info("Cwd: ~p~n",[Cwd]),
    cmd(Cwd, Params, Config).

cmd(Cwd,["files", Path | Params], _Conf) ->
    Dir = filename:join([Cwd , Path]),
    {IsNaga, Opts} = opts(Dir),
    cmd_files(IsNaga, Dir, Opts, Params);
cmd(Cwd,["modules", Path | Params], _Conf) ->
    Dir = filename:join([Cwd , Path]),
    {IsNaga, Opts} = opts(Dir),
    case IsNaga of true -> 
    [begin case modules(T, Dir, Opts) of []-> false;
               Modules ->[io:format("~p,~p~n",[T,F])||F<-Modules] end 
     end || T <- Params]; _ -> [] end.
      
cmd_files(false,_,_,_) -> false;
cmd_files(true, _, _, []) -> false; 
cmd_files(true, Dir, Opts, [P|T]) ->
    [mad:info("~p,~p~n",[P,F])||F<-files(P, Dir, Opts)],
    cmd_files(true, Dir, Opts, T).
   
%% naga_cmd(_, Cwd,["build", App], Opts) -> 
%%     io:format("~n---- naga build app:~n~p~n",[App]), false;
%% naga_cmd(_, Cwd,["build", App, File], Opts) ->
%%     io:format("~n---- naga build file:~n~p~n",[File]), false;
%% naga_cmd(false, _,["opts", App], Opts) ->
%%     Dir = case code:priv_dir(list_tp_atom(App)) of
%%               {error, _} ->
%%     io:format("~n---- naga options:~n~p~n",[Opts]);

%% naga_cmd(false, _, _, _) -> 
%%     io:format("Not a naga app.~n"),false;
%% naga_cmd(true, _,["opts"], Opts) ->
%%     io:format("~n---- naga options:~n~p~n",[Opts]);

opts(Dir) ->    
    Config = mad_utils:consult(filename:join([Dir, "rebar.config"])),
    opts(Dir, Config).    
opts(Dir, Config) ->    
    Name = app_name(Dir),
    %mad:info("Name ~p~n",[Name]),
    case is_naga(Name, Config) of
        true -> {{_, NagaOpts}, _} = get_kv(naga_opts, Config, []),
                {true, overide(naga_default_opts(),NagaOpts)};
        false -> {false,Config}
    end.

get_kv(K, Opts, Default) ->
    V = mad_utils:get_value(K, Opts, Default),KV = {K, V},{KV, Opts -- [KV]}.

overide(Opts,[]) -> Opts;
overide(L1,[{K,V}|T]) -> overide(lists:keyreplace(K,1, L1, {K,V}),T). 

file_to_beam(Bin, Filename) -> filename:join(Bin, filename:basename(Filename) ++ ".beam").
