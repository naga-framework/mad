-module(error).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").

'404'(_, _, _)   -> {ok, [{msg, "404 Not Found"}]}.

event(Event) -> wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).
