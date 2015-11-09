-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").

hello(<<"GET">>, _, _)   -> {ok, [{msg, "Hello World!!!!!"}]}.

event(Event) -> wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).
