-module(mad).
-copyright('Maxim Sokhatsky').
-include("mad.hrl").
-compile(export_all).
-export([main/1]).

main([])          -> help();
main(Params)      ->
    {Other,F}      = mad_utils:fold_params(Params),
    io:format("Params: ~p~n",[{Other,F}]),
    unknown(Other),
    return(lists:any(fun(X) -> element(1,X) == error end,
           lists:flatten(
           lists:foldl(
        fun ({Name,Par},Errors) when length(Errors) > 0 -> [{error,Errors}];
            ({Name,Par},Errors) -> lists:flatten([errors((profile()):Name(Par))|Errors]) end, [], F)))).

naga(["create"|_]=Params) -> mad_tpl:app(Params);   
naga(Params)      -> mad_naga:cmd(Params).  
deps(Params)      -> mad_deps:deps(Params).
compile(Params)   -> mad_compile:compile(Params).
app(Params)       -> mad_templates:app(Params).
clean(Params)     -> mad_run:clean(Params).
start(Params)     -> mad_run:start(Params).
attach(Params)    -> mad_run:attach(Params).
stop(Params)      -> mad_run:stop(Params).
release(Params)   -> mad_release:main(Params).
ling(Params)      -> mad_ling:main(filename:basename(case Params of [] ->   mad_utils:cwd(); E -> E end)).
static(Params)    -> { _Cwd,_ConfigFileName,_Config } = configs(),          mad_static:main(_Config, Params).
sh(Params)        -> { _Cwd,_ConfigFileName,_Config } = configs(),          mad_repl:main(Params,_Config).
up(Params)        -> { _Cwd,_ConfigFileName,_Config } = configs(),          mad_deps:up(_Config,Params).

configs() ->
    Cwd            = mad_utils:cwd(),
    ConfigFile     = "rebar.config",
    ConfigFileAbs  = filename:join(Cwd, ConfigFile),
    Conf           = mad_utils:consult(ConfigFileAbs),
    Conf1          = mad_script:script(ConfigFileAbs, Conf, ""),
    {Cwd,ConfigFile,Conf1}.

unknown([])       -> skip;
unknown(Other)    -> info("Unknown: ~p~n",[Other]), help().

errors(false)     -> [];
errors(true)      -> {error,unknown};
errors({error,L}) -> info("ERR: ~tp~n",[L]), {error,L};
errors({ok,L})    -> info("OK:  ~tp~n",[L]), [];
errors(X)         -> info("RETURN: ~tp~n",[X]), {error,X}.

return(true)      -> 1;
return(false)     -> 0;
return(X)         -> X.

info(Format)      -> io:format(lists:concat([Format,"\r"])).
info(Format,Args) -> io:format(lists:concat([Format,"\r"]),Args).

help(Reason,D)    -> help(io_lib:format("~s ~p", [Reason, D])).
help(Msg)         -> help().
help()            -> info("MAD Container Tool version ~s~n",[?VERSION]),
                     info("~n"),
                     info("    invoke = mad params~n"),
                     info("    params = [] | command [ options  ] params ~n"),
                     info("   command = app     | deps  | clean | compile | up~n"),
                     info("           | release [ beam  | ling  | script  | runc | depot ]~n"),
                     info("           | deploy  | start | stop  | attach  | sh ~n"),
                     return(false).
