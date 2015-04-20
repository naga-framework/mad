-module({{appid}}_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).


-spec start(_, _) -> {ok, pid()}.
start(_, _) ->
    io:format("starting {{appid}}...~n"),
    application:start(naga),
    naga:start({{appid}}),
    {{appid}}_sup:start_link().

-spec stop(_) -> ok.
stop(_) ->
	ok.
