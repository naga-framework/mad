-module(index).
-export([index/3, event/1]).
-default_action(index).
-actions([index]).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").

index(<<"GET">>, _, _)   -> 
  {ok, [{msg, "Hello World!!!!! "}]}.

event(Event) -> 
  wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).