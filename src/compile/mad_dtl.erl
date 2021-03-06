-module(mad_dtl).
-copyright('Sina Samavati').
-compile(export_all).

-define(CUSTOM_TAGS_DIR_MODULE, '_view_lib_tags').

is_naga([])-> false;
is_naga(O) -> case proplists:get_value(naga,O) of
                undefined -> false;
                O1 -> proplists:get_value(enable,O1,false) end.

compile(Dir,Config) ->
    case mad_utils:get_value(erlydtl_opts, Config, []) of
        [] -> false;
         X -> O = validate_erlydtl_opts(Dir,X),
              case is_naga(O) of 
                false -> compile_erlydtl_files(O); 
                true  -> case compile_erlydtl_naga_files({naga,view_dir},O) of 
                          true -> true; 
                          false -> compile_erlydtl_naga_files({naga_mail,mail_dir},O) 
                         end 
              end 
    end.

get_kv(K, Opts, Default) ->
    V = mad_utils:get_value(K, Opts, Default),
    KV = {K, V},
    {KV, Opts -- [KV]}.

file_to_beam(Bin, Filename) -> filename:join(Bin, filename:basename(Filename) ++ ".beam").

validate_erlydtl_opts(Cwd, Opts) ->
    DefaultDocRoot          = filename:join("priv", "templates"),
    {{_, DocRootDir}, Opts1}= get_kv(doc_root, Opts, DefaultDocRoot),
    {{_, OutDir1}, Opts2}   = get_kv(out_dir, Opts1, "ebin"),
    {CompilerOpts, Opts3}   = get_kv(compiler_options, Opts2, []),
    {SourceExt, Opts4}      = get_kv(source_ext, Opts3, ".dtl"),
    {ModuleExt, Opts5}      = get_kv(module_ext, Opts4, ""),

    DocRoot1 = {doc_root, filename:join(Cwd, DocRootDir)},
    OutDir2 = {out_dir, filename:join(Cwd, OutDir1)},
 
    [{cwd, Cwd},DocRoot1, OutDir2, CompilerOpts, SourceExt, ModuleExt|Opts5].


module_name(File, Ext, NewExt) ->
    list_to_atom(filename:basename(File, Ext) ++ NewExt).
module_name(File, ViewsOpts) ->
    {{_, Cwd}, _} = get_kv(cwd, ViewsOpts, ""),
    F = filename:split(File -- Cwd) -- ["/"],
    %mad:info("module_name File ~p~n",[F]),
    Name = mad_naga:view_module(F,ViewsOpts),
    list_to_atom(Name).

compile_erlydtl_files(Opts) ->

    {{_, DocRoot},   Opts1} = get_kv(doc_root,   Opts,  ""),
    {{_, SourceExt}, Opts2} = get_kv(source_ext, Opts1, ""),
    {{_, ModuleExt}, Opts3} = get_kv(module_ext, Opts2, ""),
    {{_, OutDir},        _} = get_kv(out_dir,    Opts3, ""),

    Files = filelib:fold_files(DocRoot, SourceExt++"$", true,
                               fun(F, Acc) -> [F|Acc] end, []),
    Compile = fun(F) ->
        ModuleName = module_name(F, SourceExt, ModuleExt),
        BeamFile = file_to_beam(OutDir, atom_to_list(ModuleName)),
        Compiled = mad_compile:is_compiled(BeamFile, F),
        case Compiled of false ->
             mad:info("DTL Compiling ~s~n", [F -- mad_utils:cwd()]),
             Res = erlydtl:compile(F, ModuleName, Opts3),
             file:change_time(BeamFile, calendar:local_time()),
             case Res of {error,Error} -> mad:info("Error: ~p~n",[Error]);
                                    OK -> OK end;
             true -> ok end
    end,

    lists:any(fun({error,_}) -> true; ({ok,_,_}) -> false; ({ok,_}) -> false; (ok) -> false end,[Compile(F) || F <- Files]).


compile_erlydtl_naga_files({App0,D}, Opts) ->

    {{_, Naga},    O1} = get_kv(App0,    Opts, []),    
    {{_, OutDir},  O2} = get_kv(out_dir, O1, "ebin"),
    {{_, Cwd},     O3} = get_kv(cwd,     O2, ""),
    {{_, CO},      _ } = get_kv(compiler_options, O3, ""),

    Get = fun(X) -> 
            {{_, Val },  _} = get_kv(X, Naga, proplists:get_value(X, mad_naga:cfg_dtl())),
            case Val of true -> true; false-> false; [{_,_}|_]=E -> E; 
                Dir ->filename:join(Val) end end,

    case Get(enable) of true -> 
        NagaExt   = Get(extensions),
        Force     = Get(force),
        DocRoot   = filename:join(Cwd,Get(D)),
        TagModDir = filename:join(Cwd,Get(tag_dir)),
        FilterDir = filename:join(Cwd,Get(filter_dir)),
       HtmlTagsDir= filename:join(Cwd,Get(htmltags_dir)),
        CustomTags= filename:join(Cwd,Get(custom_tags)),
        Tags0     = proplists:get_value(template_tag_modules,Naga,[]),
        Filters0  = proplists:get_value(template_filter_modules,Naga,[]),
        AutoEscape= Get(auto_escape),
        App       = filename:basename(Cwd),

        OO = [ {cwd,Cwd},{app,App},{extensions, NagaExt},{view_dir, DocRoot}
              ,{htmltags_dir, HtmlTagsDir},{tag_dir, TagModDir}
              ,{filter_dir, FilterDir},{custom_tags, CustomTags}],

        TagHelpers = mad_naga:modules(tag_dir, OO), 
        FilterHelpers = mad_naga:modules(filter_dir, OO),

        ExtraTagHelpers = lists:usort(wf:config(App,template_tag_modules,[])++Tags0),
        ExtraFilterHelpers =  lists:usort(wf:config(App,template_filter_modules,[])++Filters0),

        HelperModuleName = lists:concat([App, ?CUSTOM_TAGS_DIR_MODULE]),
        HelperModule = list_to_atom(HelperModuleName),
        TagModules = ensure_helper(OutDir,TagHelpers ++ ExtraTagHelpers),
        FilterModules = ensure_helper(OutDir,FilterHelpers ++ ExtraFilterHelpers),

        mad:info("DTL ~p:TagModules    ~p~n",[App,TagModules]),
        mad:info("DTL ~p:FilterModules ~p~n",[App,FilterModules]),

        NagaOpts = [
         {cwd, Cwd},{doc_root, DocRoot},{app,App},
         {extensions, NagaExt},{out_dir, OutDir},
         {auto_escape, AutoEscape},
         {custom_filters_modules, FilterModules}
         ] ++ CO,

        All  = mad_naga:find_files(DocRoot,NagaExt),
        Tags = mad_naga:find_files(HtmlTagsDir,[{".html",[]}]),
        Views= All -- Tags,

        NagaOpts1 = case ensure_tags(Tags,HtmlTagsDir,OutDir,HelperModuleName,NagaOpts) of
          {ok, HelperModule} -> NagaOpts ++[{custom_tags_modules, TagModules ++ [HelperModule]}];
          _ -> NagaOpts ++[{custom_tags_modules, TagModules}] end,

        Compile = fun(F) ->
            ModuleName = module_name(F, NagaOpts),
            BeamFile = file_to_beam(OutDir, atom_to_list(ModuleName)),
            Compiled = mad_compile:is_compiled(BeamFile, F),
            if  Compiled =:= false orelse Force ->
                 %mad:info("DTL options ~p~n",[NagaOpts1]),
                 mad:info("DTL Compiling ~s --> ~s~n", [F -- mad_utils:cwd(), atom_to_list(ModuleName)]),
                 Res = erlydtl:compile(F, ModuleName, NagaOpts1),
                 case Res of {error,Error,_} -> mad:info("Error: ~p~n",[Error]),{error,Error};
                             {error,Error}   -> mad:info("Error: ~p~n",[Error]),{error,Error};
                                        OK   -> OK end;
                 true -> ok end
        end,

        lists:any(fun({error,_}) -> true;({ok,_,_}) -> false; ({ok,_}) -> false; (ok) -> false end,[Compile(F) || F <- Views]); 
        _ -> false end.

ensure_tags(Files, Dir, OutDir, ModuleName, Opts) ->
  Beam = file_to_beam(OutDir, ModuleName),
  Module = list_to_atom(ModuleName),
  Filename = filename:join([OutDir,ModuleName]),
  case lists:member(false, [is_compiled(Beam,F)||F<-Files]) of
    true -> Res = erlydtl:compile_dir(Dir, Module, Opts), 
            case Res of 
               {error,Err,_} -> mad:info("Error: ~p~n",[Err]),
                                 {error,Err};
               {error,Err}   -> mad:info("Error: ~p~n",[Err]),
                                 {error,Err};
                         OK  -> code:purge(Module),
                                case code:load_abs(Filename) of
                                  {module, Module} -> {ok, Module};
                                  Error -> Error
                                end 
            end;
    false-> code:purge(Module),
            case code:load_abs(Filename) of
              {module, Module} -> {ok, Module};
              Error -> Error
            end
  end.

ensure_helper(OutDir,L) ->
  [begin 
      case code:is_loaded(X) of
        {file, _Loaded} ->  X;
        false-> F = filename:join([OutDir,X]),
                case code:load_abs(F) of
                  {module, M} -> M;
                  Error -> []
                end
      end
   end || X <- L].

is_compiled(O,F) -> filelib:is_file(O) andalso (mad_utils:last_modified(O) >= mad_utils:last_modified(F)). 
