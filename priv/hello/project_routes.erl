-module({{appid}}_routes).
-include_lib("n2o/include/wf.hrl").
-include_lib("deps/naga/include/naga.hrl").

-export([init/2, finish/2]).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) ->
    Req = Ctx#cx.req,
    {Bindings,_}  =cowboy_req:bindings(Req),

    {Controller,_}=cowboy_req:binding(controller,Req),
    {Action,_}    =cowboy_req:binding(action,Req),

    Route = match(Controller,Action),
    wf:info(?MODULE, "ROUTE ~p : ~p, ~p",[Route, Controller, Action]),
    {ok, State, Ctx#cx{path=Route,module=Route#route.controller}}.


match(undefined        ,undefined)    -> #route{app={{appid}},   controller=index  ,action=hello};
match(<<"redirect">>   ,undefined)    -> #route{app={{appid}},   controller=index  ,action=redirect};
match(<<"redirect">>   ,<<"header">>) -> #route{app={{appid}},   controller=index  ,action=redirect_header};
match(<<"moved">>      ,undefined)    -> #route{app={{appid}},   controller=index  ,action=moved};
match(<<"moved">>      ,<<"header">>) -> #route{app={{appid}},   controller=index  ,action=moved_header};
match(<<"action">>     ,<<"other">>)  -> #route{app={{appid}},   controller=index  ,action=hello};
match(<<"hello">>      ,undefined)    -> #route{app={{appid}},   controller=index  ,action=hello};
match(_,_)                            -> #route{app={{appid}},   controller=error  ,action='404'}.
    