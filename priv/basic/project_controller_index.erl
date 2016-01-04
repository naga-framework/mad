-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("naga/include/naga.hrl").

redirect(<<"GET">>, _, _)   -> 
    Count = case wf:cache(count) of undefined -> 0; C -> C + 1 end,
	wf:cache(count, Count),
	wf:info(?MODULE,"ACTION redirect executed",[]),
	{redirect, "/hello"}.

redirect_header(<<"GET">>, _, _)   -> 
    Count = case wf:cache(count) of undefined -> 0; C -> C + 1 end,
	wf:cache(count, Count),
	wf:info(?MODULE,"ACTION redirect_header executed",[]),
	{redirect, "/hello", [{<<"X-NAGA-REDIRECT">>, <<"HELLO MAESTRO REDIRECT">>}]}.

moved(<<"GET">>, _, _)   -> 
    Count = case wf:cache(count) of undefined -> 0; C -> C + 1 end,
	wf:cache(count, Count),
	wf:info(?MODULE,"ACTION moved executed",[]),
	{moved, "/hello"}.

moved_header(<<"GET">>, _, _)   -> 
    Count = case wf:cache(count) of undefined -> 0; C -> C + 1 end,
	wf:cache(count, Count),
	wf:info(?MODULE,"ACTION moved_header executed",[]),
	{moved, "/hello", [{<<"X-NAGA-MOVED">>, <<"HELLO MAESTRO MOVED">>}]}.

action_other(<<"GET">>, _, _)   -> 
    Count = case wf:cache(count) of undefined -> 0; C -> C + 1 end,
	wf:cache(count, Count),
	wf:info(?MODULE,"ACTION action_other executed",[]),
    {action_other, #route{app=toto,controller=index,action=hello}}.

render_other(<<"GET">>, _, _)   -> 
    Count = case wf:cache(count) of undefined -> 0; C -> C + 1 end,
	wf:cache(count, Count),
    wf:info(?MODULE,"ACTION render_other executed",[]),
    {render_other, #route{app=toto,controller=index,action=hello}}.


hello(<<"GET">>, _, _)   -> 
    Count = case wf:cache(count) of undefined -> 0; C -> C + 1 end,
	wf:cache(count, Count),
	{ok, [{msg, "Hello World!!!!! " ++ nitro:to_list(wf:cache(count))}]}.

event(Event) -> 
	wf:info(?MODULE,"Unknown Event: ~p~n",[Event]).
