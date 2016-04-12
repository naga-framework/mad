-module({{appid}}_index).
-export([index/3]).

-default_action(index).
-actions([index]).

index(<<"GET">>, [], _ReqCtx) ->
 {ok, [{msg, "Hello World"}]}.
