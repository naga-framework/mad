-module({{appid}}_index_controller, [Req, SessionId]).
-export([index/3]).
-default_action(indeX).


index(<<"GET">>, [], _ReqCtx) ->
 {ok, [{msg, "Hello World"}]}.
