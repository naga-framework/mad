-module({{appid}}_routes).
-include_lib("n2o/include/wf.hrl").
-include_lib("deps/naga/include/naga.hrl").

-export([init/2, finish/2]).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) ->
    Req = Ctx#cx.req,
    {Bindings, _} =cowboy_req:binding(bindings,Req), 
    {Controller,_}=cowboy_req:binding(controller,Req),
    {Action,_}    =cowboy_req:binding(action,Req),
    {Method,_}    =cowboy_req:method(Req),
    {Params,_}    =cowboy_req:path_info(Req),

    {C,A} = match(Controller,Action),
    R = #{application => {{appid}},
          method      => Method,
          controller  => C,  %%module name        
          action      => A,
          params      => Params,
          bindings    => Bindings
         },
    %wf:info(?MODULE, "ROUTE ~p : ~p, ~p",[wf:path(Req), C, A]),
    {ok, State, Ctx#cx{path=R,module=C}}.

match(undefined        ,undefined)    -> {index ,hello};
match(<<"redirect">>   ,undefined)    -> {index ,redirect};
match(<<"redirect">>   ,<<"header">>) -> {index ,redirect_header};
match(<<"moved">>      ,undefined)    -> {index ,moved};
match(<<"moved">>      ,<<"header">>) -> {index ,moved_header};
match(<<"action">>     ,<<"other">>)  -> {index ,hello};
match(<<"hello">>      ,undefined)    -> {index ,hello};
match(_,_)                            -> {error ,'404'}.
