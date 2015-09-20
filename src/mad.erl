-module(mad).
-copyright('Maxim Sokhatsky').
-include("mad.hrl").
-compile(export_all).
-export([main/1]).

main([]) -> help();
main(Params) ->
    {Other,FP} = mad_utils:fold_params(Params),
    mad:info("code:lib_dir/0: ~p~n",[code:lib_dir()]),
    case Other == [] of
         true -> skip;
         false -> mad:info("Unknown Command or Parameter ~p~n",[Other]), help() end,

    Cwd           = mad_utils:cwd(),
    ConfigFile    = "rebar.config",
    ConfigFileAbs = filename:join(Cwd, ConfigFile),
    Conf          = mad_utils:consult(ConfigFileAbs),
    Conf1         = mad_script:script(ConfigFileAbs, Conf, ""),

    return(bool(lists:foldl(fun (_,true) -> true;
          ({Name,Par},false) -> ?MODULE:Name(Cwd, ConfigFile, Conf1, Par) end, false, FP))).

bool(false) -> 0;
bool(_) -> 1.


%% fetch dependencies
deps(Cwd, ConfigFile, Conf, Params) ->
    mad:info("Deps Params: ~p~n",[Params]),
    case mad_utils:get_value(deps, Conf, []) of
        [] -> false;
        Deps ->
            Cache = mad_utils:get_value(deps_dir, Conf, deps_fetch),
            case Cache of
                deps_fetch -> skip;
                Dir -> file:make_dir(Dir) end,
            FetchDir = mad_utils:get_value(deps_dir, Conf, ["deps"]),
            file:make_dir(FetchDir),
            mad_deps:fetch(Cwd, Conf, ConfigFile, Deps)
    end.

%% compile dependencies and the app
compile(Cwd, ConfigFile, Conf, Params) ->
    mad:info("Compile Params: ~p~n",[Params]),
    Res = case Params of
              [] -> mad_compile:'compile-deps'(Cwd, ConfigFile, Conf);
              __ -> mad_compile:deps(Cwd, Conf, ConfigFile, Params)
    end,    
    case Res of
        true -> true;
        false -> mad_compile:'compile-apps'(Cwd, ConfigFile, Conf) end.

%% reltool apps resolving
plan(_Cwd,_ConfigFileName,_Config,Params) ->
    mad:info("Plan Params: ~p~n",[Params]),
    mad_plan:main([]).

repl(_Cwd,_ConfigFileName,_Config,Params) ->
    mad:info("REPL Params: ~p~n",[Params]),
    mad_repl:main(Params,_Config).

bundle(_Cwd,_ConfigFileName,_Config,Params) ->
    mad:info("Bundle Params: ~p~n",[Params]),
    Name = case Params of [] -> mad_utils:cwd(); E -> E end,
    mad_bundle:main(filename:basename(Name)).

up(_Cwd,_ConfigFileName,_Config,Params) ->
    mad:info("Up Params: ~p~n",[Params]),
    mad_deps:up(_Config,Params).

ling(_Cwd,_ConfigFileName,_Config,Params) ->
    mad:info("Ling Params: ~p~n",[Params]),
    Name = case Params of [] -> mad_utils:cwd(); E -> E end,
    mad_ling:main(filename:basename(Name)).

app(_Cwd,_ConfigFileName,_Config,Params) ->
    mad:info("Create App Params: ~p~n",[Params]),
    mad_create:app(Params).

lib(_Cwd,_ConfigFileName,_Config,Params) ->
    mad:info("Create Lib Params: ~p~n",[Params]),
    mad_create:lib(Params).

clean(_Cwd,_ConfigFileName,_Config,Params) ->
    mad:info("Clean Params: ~p~n",[Params]),
    mad_run:clean(Params).

start(_Cwd,_ConfigFileName,_Config,Params) ->
    mad:info("Start Params: ~p~n",[Params]),
    mad_run:start(Params), false.

attach(_Cwd,_ConfigFileName,_Config,Params) ->
    mad_run:attach(Params), false.

stop(_Cwd,_ConfigFileName,_Config,Params) ->
    mad:info("Stop Params: ~p~n",[Params]),
    mad_run:stop(Params), false.

release(_Cwd,_ConfigFileName,_Config,Params) ->
    mad:info("Release Params: ~p~n",[Params]),
    mad_release:main(Params).

static(_Cwd,_ConfigFileName,Config,Params) ->
    mad:info("Compile Static Params: ~p~n",[Params]),
    mad_static:main(Config, Params).

lock_deps(_Cwd,_ConfigFileName,Config,Params) ->
    io:format("Compile Lock-deps Params: ~p~n",[Params]),
    mad_lock:'lock-deps'(Config, Params).

dtl(_Cwd,_ConfigFileName,_Config,["strings", Path]) ->
    %%FIXME: find erlydtl from config
    code:replace_path(erlydtl, "./deps/erlydtl/ebin"),
    case filelib:is_dir(Path) of
        true ->
            case mad_naga:opts(Path) of
                {true, Opts} -> 
                    Files = mad_naga:files(view, Path, Opts), 
                    [begin 
                         %FF = case hd(F) of $/ -> F; _ -> filename:join([_Cwd, F]) end,
                         print_str(F)
                     end||F<-Files],false;
                _ ->
                    io:format("Not Naga App~n"), 
                    true
            end;
        false ->
            %FilePath = case hd(Path) of $/ -> Path; _ -> filename:join([_Cwd, Path]) end,
            print_str(Path),
            false
    end.

    
print_str(File) ->
    case catch sources_parser:parse_file(File) of
        {error, _} -> skip;
        List -> R = lists:reverse(List),
                Sep = ",",
                [begin
                     [_F, Str, Line, Col] = sources_parser:phrase_info([file, msgid, line, col], P), 
                     io:format("~p~s~ts~s~p~s~p~n",[File,Sep, Str,Sep, Line,Sep, Col])
                 end || P <- R, is_tuple(P)]
    end.
             
naga(_Cwd,_ConfigFileName,_Config,["create" | _]=Params)->
    %io:format("Create Naga App Params: ~p~n",[Params]),
    mad_tpl:app(Params), false;
    
naga(Cwd,_ConfigFileName,Config,Params)->
    %io:format("naga ~p~n",[Params]),
    mad_naga:cmd(Cwd,_ConfigFileName,Config,Params).

version() -> ?VERSION.
help(Reason, Data) -> help(io_lib:format("~s ~p", [Reason, Data])).
help(Msg) -> mad:info("Error: ~s~n~n", [Msg]), help().
help() ->
    mad:info("MAD Build Tool version ~s~n",[version()]),
    mad:info("BNF: ~n"),
    mad:info("    invoke := mad params~n"),
    mad:info("    params := [] | run params ~n"),
    mad:info("       run := command [ options ]~n"),
    mad:info("   command := app | lib | deps | up | compile | release | bundle~n"),
    mad:info("              clean | start | stop | attach | repl | dtl | naga | lock-deps ~n"),
    mad_naga:help(),
    return(0).

info(Format) -> io:format(lists:concat([Format,"\r"])).
info(Format,Args) -> io:format(lists:concat([Format,"\r"]),Args).

return(X) -> X.
