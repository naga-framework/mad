-module({{appid}}_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-spec start(_, _) -> {ok, pid()}.
start(_, _) ->
    error_logger:info_msg("Starting application {{appid}}...", []),
    {{appid}}:start_db(),
    {{appid}}_sup:start_link().

-spec stop(_) -> ok.
stop(_) ->
	ok.
