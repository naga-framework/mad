-module({{appid}}).
-export([start/0, stop/0]).

start() ->
    application:ensure_all_started({{appid}}).

stop() ->
    application:stop({{appid}}).

start_db() ->       
    io:format("Starting {{appid}} db...~n"),
    DBOptions = lists:foldl(fun(OptName, Acc) ->
                case application:get_env({{appid}}, OptName) of
                    {ok, Val} -> [{OptName, Val}|Acc];
                    _ -> Acc
                end
        end, [], [db_port, db_host, db_username, db_password, db_database, 
            db_replication_set, db_read_mode, db_write_mode, 
            db_write_host, db_write_host_port, db_read_capacity, 
            db_write_capacity, db_model_read_capacity, db_model_write_capacity]),

    DBAdapter = get_env(db_adapter, mock),
    DBShards = get_env(db_shards, []),
    CacheEnable = get_env(cache_enable, false),
    IsMasterNode = get_env(is_master_node, true),
    CachePrefix = get_env(cache_prefix, db),
    DBCacheEnable = get_env(db_cache_enable, false) andalso CacheEnable,
    DBOptions1 = [{adapter, DBAdapter}, 
                  {cache_enable, DBCacheEnable}, 
                  {cache_prefix, CachePrefix},
                  {shards, DBShards}, {is_master_node, IsMasterNode}|DBOptions],

    boss_db:start(DBOptions1),
    boss_news:start(),

    case get_env(cache_enable, false) of
        false -> ok;
        true  ->
            CacheAdapter = get_env(cache_adapter, memcached_bin),
            CacheOptions = cache_options(CacheAdapter),
            boss_cache:start(CacheOptions)
    end.
    
   
cache_options(ets) ->
    [{adapter, ets}, 
     {ets_maxsize, get_env(ets_maxsize, 32 * 1024 * 1024)},
     {ets_threshold, get_env(ets_threshold, 0.85)}, 
     {ets_weight, get_env(ets_weight, 30)}];

cache_options(memcached_bin) ->
    [
     {adapter, memcached_bin},
     {cache_servers, get_env(cache_servers, [{"127.0.0.1", 11211, 1}])}
    ];

cache_options(redis) ->
    [
     {host, "localhost"},
     {port, 6379},
     {pass, undefined},
     {db, 0},
     {reconnect, true}
    ].

get_env(App, Key, Default) when is_atom(App), is_atom(Key) ->
    case application:get_env(App, Key) of
        {ok, Val} -> Val;
        _ -> Default
    end.

get_env(Key, Default) when is_atom(Key) ->
    get_env({{appid}}, Key, Default).
