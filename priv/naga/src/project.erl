-module({{appid}}).
-export([start/0, stop/0]).

start() ->
    io:format("Starting {{appid}}...~n"),
    application:start(naga),
    application:start({{appid}}),
    naga:start({{appid}}).


stop() ->
    naga:stop({{appid}}),
    application:stop({{appid}}),
    application:stop(naga).
