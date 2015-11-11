-module(mad_naga).
-copyright('Chan Sisowath').
-compile(export_all).

get_kv(K, Opts, Default) ->
    V = mad_utils:get_value(K, Opts, Default),
    KV = {K, V},
    {KV, Opts -- [KV]}.

cfg_dft() ->
    [
     {enable,          false} %% isNaga
    ,{auto_escape,     false}  
    ,{extensions,      [{".html","_html"},{".js", "_js"},{".css","_css"},{".json","_json"},{".txt","_txt"}]}
    ,{controller_dir,  ["src","controller"]}
    ,{view_dir,        ["src","view"]}
    ,{tag_dir,         ["src","view","lib", "tag_modules"]}
    ,{filter_dir,      ["src","view","lib", "filter_modules"]}
    ,{htmltags_dir,    ["src","view","lib", "tag_html"]}
    ,{custom_tags,     ["src","view","lib", "custom_tags"]}
    ].

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
    Files = mad_compile:files(TypeDir,Ext),[(F--Cwd)--"/"||F<-Files];

files(Type, Opts) -> files({Type,".erl"}, Opts).

views(Type, Opts) ->
    {{_, Exts}, Opts1}    = get_kv(extensions, Opts, ""),
    {{_, TypeDir}, Opts2} = get_kv(Type, Opts1, ""),
    {{_, Cwd}, Opts3}     = get_kv(cwd, Opts2, ""),
    Files = lists:foldl(fun({Ext,_},Acc) -> mad_compile:files(TypeDir,Ext) ++ Acc end, 
                        [], Exts),[(F--Cwd)--"/"||F<-Files].

modules(view_dir, O)     -> [view_module(filename:split(F),O)||F<-files(view_dir,O)];
modules(htmltags_dir, O) -> [view_module(filename:split(F),O)||F<-files(htmltags_dir,O)];
modules(Type, O)         -> [list_to_atom(filename:rootname(filename:basename(F)))||F<-files(Type,O)].

