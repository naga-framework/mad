-module(mad_boss).
-copyright('Chan Sisowath').
-compile(export_all).

erl_to_beam(Bin, F) -> filename:join(Bin, filename:basename(F, ".erl") ++ ".beam").

get_kv(K, Opts, Default) ->
    V = mad_utils:get_value(K, Opts, Default),
    KV = {K, V},
    {KV, Opts -- [KV]}.

compile(Dir,Config,Inc,Deps) ->

    case mad_utils:get_value(bossdb_opts, Config, []) of
        [] -> false;
         X -> compile_boss_files(validate_boss_opts(Dir,X), Inc, Deps) end.

validate_boss_opts(Cwd, Opts) ->
    {{_, OutDir1}, Opts1}   = get_kv(out_dir, Opts, "ebin"),
    OutDir2 = {out_dir, filename:join(Cwd, OutDir1)},
    [{cwd, Cwd},OutDir2|Opts1].

compile_boss_files(Opts, Inc, Deps) ->
    {{_, Cwd},        O1} = get_kv(cwd, Opts, ""),
    {{_, ModelDir},   O2} = get_kv(model_dir,     O1,  filename:join(["src","model"])),
    {{_, Manager},    O3} = get_kv(model_manager, O2, none),
    {{_, OutDir},      _} = get_kv(out_dir,    O3, ""),

    case Manager of none -> false; boss_db ->
    ErlOpts = proplists:get_value(erl_opts, Opts, []),
    OO = [debug_info,
              {pre_revert_transform, fun boss_record_compiler:trick_out_forms/2},
              {token_transform,      fun boss_record_compiler:process_tokens/1}
            , {i, Inc}, ErlOpts++Deps, {out_dir, OutDir}],
    Dir = filename:join(Cwd,ModelDir),
    Files = filelib:fold_files(Dir, ".erl", true,fun(F, Acc) -> [F|Acc] end, []),

    Compile = fun(F) ->
        BeamFile = erl_to_beam(OutDir, F),
        Compiled = mad_compile:is_compiled(BeamFile, F),
        case Compiled of false ->
             mad:info("BOSS MODEL Compiling ~s~n", [F -- mad_utils:cwd()]),
             Res = boss_record_compiler:compile(F, OO),
             case Res of {error,Error} -> mad:info("Error: ~p~n",[Error]);
                                    OK -> OK end;
             true -> ok end
    end,

    lists:any(fun({error,_}) -> true; 
                 ({ok,_}) -> false;
                 (ok) -> false end,[Compile(F) || F <- Files]) end.
