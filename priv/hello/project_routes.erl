-module({{appid}}_routes).
-include_lib("n2o/include/wf.hrl").
-include_lib("deps/naga/include/naga.hrl").

-export([init/2, finish/2]).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) ->
    Req = Ctx#cx.req,
    {Bindings,_}  =cowboy_req:bindings(Req),
    %%{App,_}=cowboy_req:binding(controller,Req),
    {Controller,_}=cowboy_req:binding(controller,Req),
    {Action,_}    =cowboy_req:binding(action,Req),

    Route = match(Controller,Action),
    wf:info(?MODULE, "ROUTE ~p : ~p, ~p, ~p",[Route, Controller, Action]),
    {ok, State, Ctx#cx{path=Route,module=Route#route.controller}}.


match(undefined        ,undefined) -> #route{app={{appid}},   controller=index  ,action=hello};
match(<<"index">>      ,undefined) -> #route{app={{appid}},   controller=index  ,action=hello};
match(<<"index.html">> ,undefined) -> #route{app={{appid}},   controller=index  ,action=hello};
match(_,_)                         -> #route{app={{appid}},   controller=error  ,action='404'}.
  