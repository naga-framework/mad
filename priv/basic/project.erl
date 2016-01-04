-module({{appid}}).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1]).
-compile(export_all).

main(A)    -> mad_repl:sh(A).
start(_,_) -> supervisor:start_link({local,{{appid}} }, {{appid}},[]).
stop(_)    -> ok.
init([])   -> naga:start({{appid}}), 
              naga:watch({{appid}}),
              sup().
sup()      -> { ok, { { one_for_one, 5, 100 }, [] } }.

log_modules() -> [n2o_client,n2o_nitrogen,n2o_stream,wf_convert,index,error,{{appid}},{{appid}}_routes].
