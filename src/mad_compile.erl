-module(mad_compile).
-copyright('Sina Samavati').
-compile(export_all).

%% compile dependencies
deps(_, _, _, []) -> ok;
deps(Cwd, Conf, ConfigFile, [H|T]) ->
    {Name, _} = mad_deps:name_and_repo(H),
    case get(Name) == compiled andalso get(mode) /= active  of
        true -> ok;
        _ -> dep(Cwd, Conf, ConfigFile, Name) end,
    deps(Cwd, Conf, ConfigFile, T).

%% compile a dependency
dep(Cwd, _Conf, ConfigFile, Name) ->
    %% check dependencies of the dependency
    DepsDir = filename:join([mad_utils:get_value(deps_dir, _Conf, ["deps"])]),
    DepPath = filename:join([Cwd, DepsDir, Name]),

    DepConfigFile = filename:join(DepPath, ConfigFile),
    Conf = mad_utils:consult(DepConfigFile),
    io:format("==> ~p~n\r",[Name]),

    Conf1 = mad_script:script(DepConfigFile, Conf, Name),
    deps(Cwd, Conf, ConfigFile, mad_utils:get_value(deps, Conf1, [])),

    SrcDir = filename:join([mad_utils:src(DepPath)]),
    %io:format("DepPath ==> ~p~n\r",[DepPath]),

    apply_hooks(pre_hooks, Conf, Cwd, DepPath),

    case mad_naga:is_naga(Name, Conf) of false ->
    Files = files(SrcDir,".yrl") ++ 
            files(SrcDir,".xrl") ++ 
            files(SrcDir,".lfe") ++ 
            files(SrcDir,".erl") ++ % comment this to build with erlc/1
            files(SrcDir,".app.src"),

    case Files of
        [] -> ok;
        Files ->
            IncDir = mad_utils:include(DepPath),
            EbinDir = mad_utils:ebin(DepPath),

            %% create EbinDir and add it to code path
            file:make_dir(EbinDir),
            code:replace_path(Name,EbinDir),

            %erlc(DepPath), % comment this to build with files/2

            Opts = mad_utils:get_value(erl_opts, Conf1, []),
            lists:foreach(compile_fun(IncDir, EbinDir, Opts), Files),

            mad_dtl:compile(DepPath,Conf1),
            mad_port:compile(DepPath,Conf1),

            put(Name, compiled),
            ok
    end;
        true ->
            EbinDir = mad_utils:ebin(DepPath),
            file:make_dir(EbinDir),
            code:replace_path(Name,EbinDir),
            mad_naga:compile(DepPath,Conf1),
            put(Name, compiled),
            ok
    end.


compile_fun(Inc,Bin,Opt) -> fun(File) -> (module(filetype(File))):compile(File,Inc,Bin,Opt) end.

module("erl") -> mad_erl;
module("lfe") -> mad_lfe;
module("erl.src") -> mad_utils;
module("yrl") -> mad_yecc;
module("xrl") -> mad_leex;
module("app.src") -> mad_app;
module(_) -> mad_none.

filetype(Path) -> string:join(tl(string:tokens(filename:basename(Path), ".")), ".").
files(Dir,Ext) -> filelib:fold_files(Dir, Ext, true, fun(F, Acc) -> [F|Acc] end, []).
is_compiled(BeamFile, File) -> mad_utils:last_modified(BeamFile) >= mad_utils:last_modified(File).

'compile-apps'(Cwd, ConfigFile, Conf) ->
    Dirs = mad_utils:sub_dirs(Cwd, ConfigFile, Conf),
    case Dirs of
        [] -> mad_compile:dep(Cwd, Conf, ConfigFile, Cwd);
        Apps -> mad_compile:deps(Cwd, Conf, ConfigFile, Apps) end.

'compile-deps'(Cwd, ConfigFile, Conf) ->
    mad_compile:deps(Cwd, Conf, ConfigFile, mad_utils:get_value(deps, Conf, [])).

list(X) when is_atom(X) -> atom_to_list(X);
list(X) -> X.

erlc(DepPath) ->
    ErlFiles = filelib:wildcard(DepPath++"/src/**/*.erl"),
    io:format("Files: ~s~n\r",[[filename:basename(Erl)++" " ||Erl<-ErlFiles]]),
    {_,Status,X} = sh:run("erlc",["-o"++DepPath++"/ebin/","-I"++DepPath++"/include"]++
        ErlFiles,binary,filename:absname("."),[{"ERL_LIBS","apps:deps"}]),
    case Status == 0 of
         true -> skip;
         false -> io:format("Error: ~s~n\r",[binary_to_list(X)]) end.


%% --- hooks -- %%
apply_hooks(Mode, Config, Cwd, DepPath) ->
    Hooks = mad_utils:get_value(Mode, Config, []),
    Default = filename:join(Cwd, "deps"),
    DepsDir = filename:join(Cwd, mad_utils:get_value(deps_dir, Config, [Default])), 
    Env = [{"REBAR_DEPS_DIR",DepsDir}],   
    lists:foreach(fun apply_hook/1, [{Env, DepPath, H}||H<-Hooks]).

apply_hook({Env, DepPath, {Arch, Command, Hook}}) ->
    case is_arch(Arch) of
        true ->
            apply_hook({Env, DepPath, {Command, Hook}});
        false ->
            ok
    end;
apply_hook({Env, DepPath, {Command, Hook}}) ->
    {_,Status,X} = sh:run("/bin/sh", ["-c", Hook], binary, DepPath, Env),    
    case Status == 0 of
         true -> 
            %%io:format("PRE HOOKS ~p~n",[X]),
            skip;
         false -> io:format("Shell Error ~p: ~s~n\r",
                            [Command, binary_to_list(X)]), exit({error,X}) 
    end.

wordsize() ->
    try erlang:system_info({wordsize, external}) of
        Val ->
            integer_to_list(8 * Val)
    catch
        error:badarg ->
            integer_to_list(8 * erlang:system_info(wordsize))
    end.

is_arch(ArchRegex) ->
    case re:run(get_arch(), ArchRegex, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.

get_arch() ->
    Words = wordsize(),
    otp_release() ++ "-"
        ++ erlang:system_info(system_architecture) ++ "-" ++ Words.


otp_release() ->
    otp_release1(erlang:system_info(otp_release)).

%% If OTP <= R16, otp_release is already what we want.
otp_release1([$R,N|_]=Rel) when is_integer(N) ->
    Rel;
%% If OTP >= 17.x, erlang:system_info(otp_release) returns just the
%% major version number, we have to read the full version from
%% a file. See http://www.erlang.org/doc/system_principles/versions.html
%% Read vsn string from the 'OTP_VERSION' file and return as list without
%% the "\n".
otp_release1(Rel) ->
    File = filename:join([code:root_dir(), "releases", Rel, "OTP_VERSION"]),
    {ok, Vsn} = file:read_file(File),

    %% It's fine to rely on the binary module here because we can
    %% be sure that it's available when the otp_release string does
    %% not begin with $R.
    Size = byte_size(Vsn),
    %% The shortest vsn string consists of at least two digits
    %% followed by "\n". Therefore, it's safe to assume Size >= 3.
    case binary:part(Vsn, {Size, -3}) of
        <<"**\n">> ->
            %% The OTP documentation mentions that a system patched
            %% using the otp_patch_apply tool available to licensed
            %% customers will leave a '**' suffix in the version as a
            %% flag saying the system consists of application versions
            %% from multiple OTP versions. We ignore this flag and
            %% drop the suffix, given for all intents and purposes, we
            %% cannot obtain relevant information from it as far as
            %% tooling is concerned.
            binary:bin_to_list(Vsn, {0, Size - 3});
        _ ->
            binary:bin_to_list(Vsn, {0, Size - 1})
    end.
