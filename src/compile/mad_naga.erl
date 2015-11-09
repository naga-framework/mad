-module(mad_naga).
-copyright('Chan Sisowath').
-compile(export_all).

get_kv(K, Opts, Default) ->
    V = mad_utils:get_value(K, Opts, Default),
    KV = {K, V},
    {KV, Opts -- [KV]}.

cfg_dft() ->
    [
     {model_manager,     none} %% boss | none     
    ,{mail_tpl_extension,[".html", ".txt"]}
    ,{controller_dir,    "src/controller"}
    ,{mail_dir,          "src/mail"}
    ,{model_dir,         "src/model"}
    ,{mail_view_dir,     "src/mail/view"}
    ,{view_dir,          "src/view"}
    ,{view_lib_dir,      "src/view/lib"}
    ,{view_tag_dir,      "src/view/lib/tag_modules"}
    ,{view_filter_dir,   "src/view/lib/filter_modules"}
    ,{view_htmltags_dir, "src/view/lib/tag_html"}
    ,{custom_tags,       "priv/custom_tags"}
    ].

view_module([_, App, "src"| C],O) ->
    Lc = string:to_lower(lists:concat([App, "_",string:join(C, "_")])),
    filename:rootname(Lc) ++ proplists:get_value(filename:extension(Lc),O).  


%% --------------------------------------------------------------------------------------
%%  
%% --------------------------------------------------------------------------------------

files(Dir,Ext) -> mad_compile:files(Dir,Ext).
files(view, Dir, Opts) ->
    {{_, SourceExt}, Opts1} = get_kv(source_ext, Opts, ""),
    {{_, ModuleExt}, Opts2} = get_kv(module_ext, Opts1, ""),
    {{_, Views },        _} = get_kv(naga,       Opts2, []),
    O = [{SourceExt,ModuleExt}]++Views,
    {ok, Cwd} = file:get_cwd(),
    Files = lists:foldl(fun({Ext,_},Acc) -> 
     					files(filename:join([Dir,"src","view"]),Ext) 
     					++ Acc end, [], O),[(F--Cwd)--"/"||F<-Files];

files(Type, Dir, Opts) ->
    {ok, Cwd} = file:get_cwd(),
    Files = files(filename:join([Dir,"src",atom_to_list(Type)]),".erl").

%% --------------------------------------------------------------------------------------
%%  
%% --------------------------------------------------------------------------------------
modules(view, Dir, Opts) ->
    {{_, SourceExt}, Opts1} = get_kv(source_ext, Opts, ""),
    {{_, ModuleExt}, Opts2} = get_kv(module_ext, Opts1, ""),
    {{_, Views },        _} = get_kv(naga,       Opts2, []),
    O = [{SourceExt,ModuleExt}]++Views,
  [view_module(filename:split(F),O)||F<-files(view,Dir,Opts)];
modules(controller, Dir, Opts) ->
  [list_to_atom(filename:rootname(filename:basename(F)))||F<-files(controller,Dir,Opts)].

%% --------------------------------------------------------------------------------------
%%  
%% --------------------------------------------------------------------------------------

