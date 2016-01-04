-module(mad_naga).
-copyright('Chan Sisowath').
-compile(export_all).

get_kv(K, Opts, Default) ->
    V = mad_utils:get_value(K, Opts, Default),
    KV = {K, V},
    {KV, Opts -- [KV]}.

cfg_dtl() ->
    [
     {enable,          false}
    ,{force,           false}  
    ,{auto_escape,     false}  
    ,{extensions,      [{".html","_html"},{".js", "_js"},{".css","_css"},{".json","_json"},{".txt","_txt"}]}
    ,{controller_dir,  ["src","controller"]}
    ,{view_dir,        ["src","view"]}
    ,{mail_dir,        ["src","mail"]}
    ,{mail_extension,  [{".html","_view_html"}, {".txt","_view_txt"}]}
    ,{tag_dir,         ["src","view","lib", "tag_modules"]}
    ,{filter_dir,      ["src","view","lib", "filter_modules"]}
    ,{htmltags_dir,    ["src","view","lib", "tag_html"]}
    ,{custom_tags,     ["src","view","lib", "custom_tags"]}
    ].

find_files(Path, Exts) -> lists:foldl(fun({Ext,_},Bcc) -> 
                        B = filelib:fold_files(Path, Ext++"$", true, fun(F, Acc) -> [F|Acc] end, []),
                        B++Bcc end, [], Exts).

dir(App)    -> case code:priv_dir(App) of {error,_} = E -> E;  D -> filename:dirname(D) end.    
config(Dir) -> ConfigFile = filename:join(Dir, "rebar.config"),
               Conf = mad_utils:consult(ConfigFile),
               mad_script:script(ConfigFile, Conf, "").
                              
%% -- DTL

view_module(["src"| C]=F,O) ->
    {{_,E},_} = get_kv(extensions, O, ""),
    {{_,App},_} = get_kv(app, O, undefined),
    Lc = string:to_lower(lists:concat([App, "_",string:join(C, "_")])),
    filename:rootname(Lc) ++ proplists:get_value(filename:extension(Lc),E);  

view_module([_, App, "src"| C]=F,O) ->
    {{_,E},_} = get_kv(extensions, O, ""),
    {{_,App},_} = get_kv(app, O, undefined),
    Lc = string:to_lower(lists:concat([App, "_",string:join(C, "_")])),
    filename:rootname(Lc) ++ proplists:get_value(filename:extension(Lc),E). 

files(view_dir, Opts)     -> views(view_dir, Opts);
files(htmltags_dir, Opts) -> views(htmltags_dir, Opts);
files({Type,Ext}, Opts)   ->
    {{_, TypeDir}, Opts1} = get_kv(Type, Opts, ""),
    {{_, Cwd}, Opts2}     = get_kv(cwd, Opts1, ""),
    Files = mad_compile:files(TypeDir,Ext),
    [(F--Cwd)--"/"||F<-Files];

files(Type, Opts) -> files({Type,".erl"}, Opts).

views(Type, Opts) ->
    {{_, Exts}, Opts1}    = get_kv(extensions, Opts, ""),
    {{_, TypeDir}, Opts2} = get_kv(Type, Opts1, ""),
    {{_, Cwd}, Opts3}     = get_kv(cwd, Opts2, ""),
    Files = lists:foldl(fun({Ext,_},Acc) -> mad_compile:files(TypeDir,Ext) ++ Acc end,[], Exts),
    [(F--Cwd)--"/"||F<-Files].

modules(view_dir, O)     -> [view_module(filename:split(F),O)||F<-files(view_dir,O)];
modules(htmltags_dir, O) -> [view_module(filename:split(F),O)||F<-files(htmltags_dir,O)];
modules(Type, O)         -> [list_to_atom(filename:rootname(filename:basename(F)))||F<-files(Type,O)].



sorted_files(Files) ->
    G = digraph:new(),
    [ digraph:add_vertex(G, N) || N <- Files ],
    R = case all_edges(Files) of
        [] -> Files;
        Edges ->
            [digraph:add_edge(G, A, B) || {A,B} <- Edges],
            lists:reverse(digraph_utils:topsort(G))
    end, digraph:delete(G), R.

all_edges(Files) ->
    Tmp = [{filename:basename(F), F} || F <- Files],
    all_edges(Files, Tmp, []).

all_edges([], _, Acc) -> Acc;
all_edges([H|T], Files, Acc) ->
    {ok, Fd} = case file:open(H, [read]) of 
        {error,enoent} =Err -> mad:info("Error open ~p:~p",[H,Err]);
         E->E end,
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
