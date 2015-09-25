-module(boss_legacy).
-export([compile/2]).
-export([parse_transform/2]).
-export([add_routes_to_forms/1]).
-export([format_error/1]).

-define(ERROR(R, T, F, I),
        begin
            rpt_error(R, T, F, I),
            throw({error,erl_syntax:get_pos(proplists:get_value(form,I)),{unknown,R}})
        end).

-import(erl_syntax, [clause/3, clause_patterns/1, clause_body/1,
                     clause_guard/1, match_expr/2, function_clauses/1,
                     get_pos/1, add_ann/2, get_ann/1]).

compile(File, Options) ->
    %io:format("Compile controller ~s~n", [File]), 
    compile_pmod1(File, [debug_info,{pre_revert_transform, fun ?MODULE:add_routes_to_forms/1}|Options]).

add_routes_to_forms(Forms) ->
    [{eof, _Line}|OtherForms] = lists:reverse(Forms),
    Forms1 = add_export_to_forms(lists:reverse(OtherForms)),
    add_routes_to_forms(Forms1, [], []).

add_routes_to_forms([], FormAcc, RouteAcc) ->
    RoutesFunction = function_for_routes(lists:reverse(RouteAcc)),
    lists:reverse(FormAcc, RoutesFunction);
add_routes_to_forms([{function, _, Name, Arity, Clauses} = Fxn|Rest], FormAcc, RouteAcc) 
  when Arity =:= 2; Arity =:= 3 ->
    NewRoutes = extract_routes_from_clauses(Name, Clauses),
    add_routes_to_forms(Rest, [Fxn|FormAcc], lists:reverse(NewRoutes, RouteAcc));
add_routes_to_forms([H|T], FormAcc, RouteAcc) ->
    add_routes_to_forms(T, [H|FormAcc], RouteAcc).

add_export_to_forms(Forms) ->
    add_export_to_forms(Forms, []).

add_export_to_forms([{attribute, _, module, _} = H | T], LeadingForms) ->
    lists:reverse(LeadingForms, [H, {attribute, {0, 2}, export, [{'_routes', 0}]} | T]);
add_export_to_forms([H|T], LeadingForms) ->
    add_export_to_forms(T, [H|LeadingForms]).

extract_routes_from_clauses(Name, Clauses) ->
    extract_routes_from_clauses(Name, Clauses, []).

extract_routes_from_clauses(_, [], Acc) ->
    lists:reverse(Acc);
extract_routes_from_clauses(Name, [{clause, _, 
            [_, URLTokens|_], _, _}|Rest], Acc) ->
    Route = route_from_token_ast(URLTokens),
    extract_routes_from_clauses(Name, Rest, [{Name, Route}|Acc]);
extract_routes_from_clauses(Name, [_H|T], Acc) ->
    extract_routes_from_clauses(Name, T, Acc).

route_from_token_ast(Tokens) ->
    route_from_token_ast(Tokens, []).

route_from_token_ast({cons, _, {var, _, VariableName}, T}, Acc) ->
    route_from_token_ast(T, [VariableName|Acc]);
route_from_token_ast({cons, _, {match, _, _, {var, _, VariableName}}, T}, Acc) ->
    route_from_token_ast(T, [VariableName|Acc]);
route_from_token_ast({cons, _, {string, _, String}, T}, Acc) ->
    route_from_token_ast(T, [String|Acc]);
route_from_token_ast(_, Acc) ->
    lists:reverse(Acc).

function_for_routes(Routes) ->
    [erl_syntax:function(erl_syntax:atom('_routes'),
    [erl_syntax:clause([], none, [erl_syntax:list(map_syntax_tuples(Routes))])])].

map_syntax_tuples(Routes) ->
    lists:map(fun({Name, Tokens}) ->
    erl_syntax:tuple([erl_syntax:atom(Name),erl_syntax:list(map_tokens(Tokens))])
              end, Routes).

map_tokens(Tokens) ->
    lists:map(fun (T) when is_atom(T) ->
                      erl_syntax:atom(T);
                  (T) when is_list(T) ->
                      erl_syntax:string(T)
              end, Tokens).

compile_pmod1(File, Options) ->
    %io:format("~nCompile file ~p with options ~p ~n", [File, Options]),
    IncludeDirs = ["include"] ++ proplists:get_value(include_dirs,    Options, []) ++
        proplists:get_all_values(i, compiler_options(Options)),
    TokenTransform = proplists:get_value(token_transform, Options),
    case parse(File, TokenTransform, IncludeDirs) of
        {ok, Forms, TokenInfo} ->
            handle_parse_success(File, Options, Forms, TokenInfo);
        Error = {error, _} -> Error end.

handle_parse_success(File, Options, Forms, TokenInfo) ->
    Version = otp_version(),
    CompilerOptions = compiler_options(Options),    
    Forms1 = make_new_forms(Options, Forms, TokenInfo),
    {Forms2, BossDBParseTransforms} = make_forms_by_version(Forms1,Version),
    ParseTransforms = make_parse_transforms(Options, BossDBParseTransforms),
    RevertedForms   = make_reverted_forms(CompilerOptions, Forms2, ParseTransforms),
    compile_forms1(File, Options, CompilerOptions, RevertedForms).

compiler_options(Options) ->
    proplists:get_value(compiler_options, Options, [verbose, return_errors]).

compile_forms1(File, Options, CompilerOptions, RevertedForms) ->
    case compile_forms(RevertedForms, File, CompilerOptions) of
        {ok, Module, Bin} ->
            ok = write_beam(Options, Module, Bin),
            {ok, Module};
        Error -> Error end.

write_beam(Options, Module, Bin) ->
    Dir = proplists:get_value(root_dir, Options),
    OutDir = Dir ++ "/" ++ proplists:get_value(ebin_dir, Options),
    BeamFile = filename:join([OutDir, lists:concat([Module, ".beam"])]),
    filelib:ensure_dir(BeamFile), file:write_file(BeamFile, Bin).

make_parse_transforms(Options, BossDBParseTransforms) ->
    BossDBParseTransforms ++ proplists:get_value(parse_transforms, Options, []).

otp_version() ->
    OTPVersion = erlang:system_info(otp_release),
    case OTPVersion of
        [$R, V1, V2 | _] ->  Version = list_to_integer([V1, V2]), Version;
        [V1, V2 | _] ->  list_to_integer([V1,V2]) end.

make_forms_by_version(NewForms, Version) when Version >= 16->
    %% OTP Version starting with R16A needs boss_db_pmod_pt
    %% pmod_pt needs the form to end with {eof, 0} tagged tupple
    NewForms1 = NewForms ++ [{eof,0}],
    %% pmod_pt needs the form to be in "new" format
    {erl_syntax:revert_forms(erl_syntax:revert(NewForms1)), [pmod_pt, ?MODULE]};
make_forms_by_version(NewForms, _Version) ->
    {erl_syntax:revert(NewForms), [?MODULE]}.

make_reverted_forms(CompilerOptions, NewNewForms, ParseTransforms) ->
    lists:foldl(fun(Mod, Acc) ->
                        Mod:parse_transform(Acc, CompilerOptions)
                end, NewNewForms, ParseTransforms).

make_new_forms(Options, Forms, TokenInfo) ->
    Transform = proplists:get_value(pre_revert_transform, Options),
    transform_action(Forms, TokenInfo, Transform).

transform_action(Forms, _, undefined) ->
    Forms;
transform_action(Forms, _TokenInfo, TransformFun) when is_function(TransformFun, 1) -> 
    TransformFun(Forms);
transform_action(Forms,  TokenInfo, TransformFun) when is_function(TransformFun, 2) ->
    TransformFun(Forms, TokenInfo). 

compile_forms(Forms, File, Options) ->
    case compile:forms(Forms, Options) of
        {ok, Module1, Bin} ->
            code:purge(Module1),
            load_binary(File, Module1, Bin);
        OtherError ->
            OtherError
    end.

load_binary(File, Module1, Bin) ->
    case code:load_binary(Module1, File, Bin) of
        {module, _} -> {ok, Module1, Bin};
        _ -> {error, lists:concat(["code reload failed: ", Module1])}
    end.

parse(File, TokenTransform, IncludeDirs) when is_list(File) ->
    case file:read_file(File) of
        {ok, FileContents} ->
            parse_text(File, FileContents, TokenTransform, IncludeDirs);
        Error ->
            Error
    end;

parse(File, TokenTransform, IncludeDirs) when is_binary(File) ->
    parse_text(undefined, File, TokenTransform, IncludeDirs).

parse_text(FileName, FileContents, TokenTransform, IncludeDirs) ->
    case scan_transform(FileContents) of
        {ok, Tokens} ->
            {NewTokens, TokenInfo} = transform_tokens(TokenTransform, Tokens),
            case aleppo:process_tokens(NewTokens, [{file, FileName}, {include, IncludeDirs}]) of
                {ok, ProcessedTokens} ->
                    handle_tokens(FileName, TokenInfo, ProcessedTokens);
                {error, ErrorInfo} ->
                    {error, {FileName, [ErrorInfo]}}
            end;
        {error, ErrorInfo} ->
            {error, {FileName, [ErrorInfo]}}
    end.

handle_tokens(FileName, TokenInfo, ProcessedTokens) ->
    % We have to flatten the token locations because the Erlang parser
    % has a bug that chokes on {Line, Col} locations in typed record
    % definitions
    TokensWithOnlyLineNumbers = flatten_token_locations(ProcessedTokens),
    {Forms, Errors} = parse_tokens(TokensWithOnlyLineNumbers, FileName),
    parse_has_errors(TokenInfo, Forms, Errors).

parse_has_errors(TokenInfo, Forms, []) ->
    {ok, Forms, TokenInfo};
parse_has_errors(_TokenInfo, _Forms, Errors) ->
     {error, make_parse_errors(Errors)}.

make_parse_errors(Errors) ->
    lists:map(fun(File) ->
                      {File, proplists:get_all_values(File, Errors)}
              end, proplists:get_keys(Errors)).

transform_tokens(undefined, Tokens) ->
    {Tokens, undefined};
transform_tokens(TransformFun,Tokens) when is_function(TransformFun) ->
    TransformFun(Tokens).

parse_tokens(Tokens, FileName) ->
    parse_tokens(Tokens, [], [], [], FileName).

parse_tokens([], _, FormAcc, ErrorAcc, _) ->
    {lists:reverse(FormAcc), lists:reverse(ErrorAcc)};
parse_tokens([{dot, _}=Token|Rest], TokenAcc, FormAcc, ErrorAcc, FileName) ->
    case erl_parse:parse_form(lists:reverse([Token|TokenAcc])) of
        {ok, {attribute, _, file, {NewFileName, _Line}} = AbsForm} ->
            parse_tokens(Rest, [], [AbsForm|FormAcc], ErrorAcc, NewFileName);
        {ok, {attribute, La, record, {Record, Fields}} = AbsForm} ->
            case epp:normalize_typed_record_fields(Fields) of
                {typed, NewFields} ->
                    parse_tokens(Rest, [], lists:reverse([
                                {attribute, La, record, {Record, NewFields}},
                                {attribute, La, type, {{record, Record}, Fields, []}}],
                            FormAcc), ErrorAcc, FileName);
                not_typed ->
                    parse_tokens(Rest, [], [AbsForm|FormAcc], ErrorAcc, FileName)
            end;
        {ok, AbsForm} ->
            parse_tokens(Rest, [], [AbsForm|FormAcc], ErrorAcc, FileName);
        {error, ErrorInfo} ->
            parse_tokens(Rest, [], FormAcc, [{FileName, ErrorInfo}|ErrorAcc], FileName)
    end;
parse_tokens([{eof, Location}], TokenAcc, FormAcc, ErrorAcc, FileName) ->
    parse_tokens([], TokenAcc, [{eof, Location}|FormAcc], ErrorAcc, FileName);
parse_tokens([Token|Rest], TokenAcc, FormAcc, ErrorAcc, FileName) ->
    parse_tokens(Rest, [Token|TokenAcc], FormAcc, ErrorAcc, FileName).

scan_transform(FileContents) ->
    scan_transform(FileContents, {1, 1}). 

scan_transform([], StartLocation) ->
    {ok, [{eof, StartLocation}]};
scan_transform(FileContents, StartLocation) when is_binary(FileContents) ->
    scan_transform(unicode:characters_to_list(FileContents), StartLocation);
scan_transform(FileContents, StartLocation) ->
    ScanResults = erl_scan:tokens([], FileContents, StartLocation),
    case ScanResults of
        {done, Return, Rest} ->
            case Return of
                {ok, Tokens, EndLocation} ->
                    case scan_transform(Rest, EndLocation) of
                        {ok, NewTokens} ->
                            {ok, Tokens ++ NewTokens};
                        Err -> Err
                    end;
                {eof, EndLocation} ->
                    {ok, [{eof, EndLocation}]};
                {error, ErrorInfo, _EndLocation} ->
                    case ErrorInfo of
                        {ErrorLocation, erl_scan, {illegal,character}} ->
                            {Truncated, IllegalChar, Rest1} = 
                                cut_at_location(ErrorLocation, FileContents, StartLocation),
                            scan_transform_illegal_char(StartLocation,
                                                        ErrorInfo, Truncated,
                                                        IllegalChar, Rest1);
                        ErrorInfo ->
                            {error, ErrorInfo}
                    end
            end;
         {more, Continuation1} ->
            {done, Return, eof} = erl_scan:tokens(Continuation1, eof, eof),
           scan_transform_result(Return)
    end.

scan_transform_result(Return) ->
    case Return of
        {ok, Tokens, _EndLocation} ->
            {ok, Tokens};
        {eof, EndLocation} ->
            {ok, [{eof, EndLocation}]};
        {error, ErrorInfo, _EndLocation} ->
            {error, ErrorInfo}
    end.

scan_transform_illegal_char(StartLocation, ErrorInfo, Truncated,
                            IllegalChar, Rest1) ->
    case transform_char(IllegalChar) of
        {ok, String} ->
            Transformed = Truncated ++ String ++ Rest1,
            scan_transform(Transformed, StartLocation);
        error ->
            {error, ErrorInfo}
    end.

transform_char(8800) -> % ≠
    {ok, ",'not_equals',"};
transform_char(8804) -> % ≤
    {ok, ",'le',"};
transform_char(8805) -> % ≥
    {ok, ",'ge',"};
transform_char(8712) -> % ∈
    {ok, ",'in',"};
transform_char(8713) -> % ∉
    {ok, ",'not_in',"};
transform_char(8715) -> % ∋
    {ok, ",'contains',"};
transform_char(8716) -> % ∌
    {ok, ",'not_contains',"};
transform_char(8764) -> % ∼
    {ok, ",'matches',"};
transform_char(8769) -> % ≁
    {ok, ",'not_matches',"};
transform_char(8839) -> % ⊇
    {ok, ",'contains_all',"};
transform_char(8841) -> % ⊉
    {ok, ",'not_contains_all',"};
transform_char(8745) -> % ∩
    {ok, ",'contains_any',"};
transform_char(8869) -> % ⊥
    {ok, ",'contains_none',"};
transform_char(10178) -> % ⊥ look-alike
    {ok, ",'contains_none',"};
transform_char(Char) when Char > 127 ->
    Bytes = binary_to_list(unicode:characters_to_binary([Char], unicode, utf8)),
    {ok, lists:flatten(lists:map(fun(Byte) ->
                        io_lib:format("\\x{~.16B}", [Byte])
                end, Bytes))};
transform_char(_) ->
    error.

cut_at_location({CutLine, CutCol}, FileContents, {StartLine, StartCol}) ->
    cut_at_location1({CutLine, CutCol}, FileContents, {StartLine, StartCol}, []).

cut_at_location1(_, [], _, Acc) ->
    {lists:reverse(Acc), 0, ""};
cut_at_location1({Line, Col}, [C|Rest], {Line, Col}, Acc) ->
    {lists:reverse(Acc), C, Rest};
cut_at_location1({Line, Col}, [C|Rest], {ThisLine, _}, Acc) when C =:= $\n ->
    cut_at_location1({Line, Col}, Rest, {ThisLine + 1, 1}, [C|Acc]);
cut_at_location1({Line, Col}, [C|Rest], {ThisLine, ThisCol}, Acc) ->
    cut_at_location1({Line, Col}, Rest, {ThisLine, ThisCol + 1}, [C|Acc]).

flatten_token_locations(Tokens) ->
    flatten_token_locations1(Tokens, []).

flatten_token_locations1([], Acc) ->
    lists:reverse(Acc);
flatten_token_locations1([{Type, {Line, _Col}}|Rest], Acc) ->
    flatten_token_locations1(Rest, [{Type, Line}|Acc]);
flatten_token_locations1([{Type, {Line, _Col}, Extra}|Rest], Acc) ->
    flatten_token_locations1(Rest, [{Type, Line, Extra}|Acc]);
flatten_token_locations1([Other|Rest], Acc) ->
    flatten_token_locations1(Rest, [Other|Acc]).

%% --------- PARSE TRANSFORM 
parse_transform(Forms, Options) ->
    TransformOperatorsFun = fun
        (Form, _Context) ->
            RevertedForm = erl_syntax:revert(Form),
            {call, Location, Invocation, Body} = RevertedForm,
            case Body of
                [Type, Conditions|Rest] ->
                    NewConditions = replace_operators(Conditions),
                    {call, Location, Invocation, [Type, NewConditions|Rest]};
                _ ->
                    RevertedForm
            end
    end,
    function({boss_db, find, any}, TransformOperatorsFun, 
        function({boss_db, count, 2}, TransformOperatorsFun, Forms, Options), 
        Options).

replace_operators({cons, _Location, {match, MatchLoc, LHS, RHS}, Rest}) ->
    LHSLoc = element(2, LHS),
    RHSLoc = element(2, RHS),
    {cons, LHSLoc, LHS,
        {cons, MatchLoc, {atom, MatchLoc, 'equals'},
            {cons, RHSLoc, RHS, replace_operators(Rest)}}};
replace_operators({cons, _Location, {op, OpLoc, Operator, LHS, RHS}, Rest}) when Operator =:= '>'; Operator =:= '<' ->
    Replacement = case Operator of '>' -> 'gt'; '<' -> 'lt' end,
    LHSLoc = element(2, LHS),
    RHSLoc = element(2, RHS),
    {cons, LHSLoc, LHS,
        {cons, OpLoc, {atom, OpLoc, Replacement},
            {cons, RHSLoc, RHS, replace_operators(Rest)}}};
replace_operators({cons, Location, First, Rest}) ->
    {cons, Location, First, replace_operators(Rest)};
replace_operators(Other) ->
    Other.

%%% API: function({Module, Function, Arity}, Fun, Forms, Options) ->
%%%         NewForms
%%%
%%% Forms and Options are the arguments passed to the parse_transform/2
%%% function.
%%% {Module, Function, Arity} is the function call to transform
%%% Fun(Form, Context) -> NewForm is the fun provided by the caller.
%%%
%%% Context is a property list, containing the following properties:
%%% - {file, Filename}
%%% - {module, ModuleName}
%%% - {function, FunctionName}       % name of the enclosing function
%%% - {arity, Arity :: integer()}    % arity of same
%%% - {var_names, Vars :: [atom()]}  % generated variables binding the
%%%                                  % function arguments.
%%%                                  % length(Vars) == Arity
%%%
function({_Module, _Function, _Arity} = MFA, F,
         Forms, Options) when is_function(F) ->
    parse_transform(MFA, F, Forms, Options).

parse_transform(MFA, Fun, Forms, _Options) ->
    [File|_] = [F || {attribute,_,file,{F,_}} <- Forms],
    try begin
            NewTree = xform(MFA, Fun, Forms, [{file, File}]),
            revert_tree(NewTree)
        end
    catch
        throw:{error,Ln,What} ->
            {error, [{File, [{Ln,?MODULE,What}]}], []}
    end.

revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].

format_error(Other) ->
    lists:flatten(
      io_lib:format("unknown error in parse_transform: ~p", [Other])).

xform({M,F,A}, Fun, Forms, Context0) ->
    Bef = fun(function, Form, Ctxt) ->
                  {Fname, Arity} = erl_syntax_lib:analyze_function(Form),
                  VarNames = erl_syntax_lib:new_variable_names(
                               Arity,
                               fun (N) ->
                                 list_to_atom ("_V" ++ integer_to_list (N))
                               end,
                               erl_syntax_lib:variables(Form)),
                  {Form, [{function, Fname},
                          {arity, Arity},
                          {var_names, VarNames}|Ctxt]};
             (_, Form, Context) ->
                  {Form, Context}
          end,
    Aft = fun(application, Form, Context) ->
                  case erl_syntax_lib:analyze_application(Form) of
                      {M, {F, _}} when A =:= any ->
                          add_ann(
                            bind_state,
                            Fun(Form, Context));
                      {M, {F, A}} ->
                          add_ann(
                            bind_state,
                            Fun(Form, Context));
                      {F, A} when M =:= "local" ->
                          add_ann(
                            bind_state,
                            Fun(Form, Context));
                      {F, _} when M =:= "local", A =:= any ->
                          add_ann(
                            bind_state,
                            Fun(Form, Context));
                      _ ->
                          Form
                  end;
             (function, Form, Context) ->
                  Form1 =
                      erl_syntax_lib:map_subtrees(
                        fun(Clause) ->
                                case should_i_bind(Clause) of
                                    true ->
                                        Pats = clause_patterns(Clause),
                                        CBod = clause_body(Clause),
                                        CGd = clause_guard(Clause),
                                        Pats1 =
                                            lists:zipwith(
                                              fun(V, P) ->
                                                      match_expr(v(V), P)
                                              end,
                                              proplists:get_value(
                                                var_names, Context),
                                              Pats),
                                        clause(Pats1, CGd, CBod);
                                    false ->
                                        Clause
                                end
                        end, Form),
                  Form1;
             (_, Form, _Context) ->
                  Form
          end,
    [Module] = [Mx || {attribute, _, module, Mx} <- Forms],
    transform(Forms, Bef, Aft, [{module, Module}|Context0]).

transform(Forms, Before, After, Context) ->
    F1 =
        fun(Form) ->
                Type = erl_syntax:type(Form),
                {Form1, Context1} =
                    try Before(Type, Form, Context)
                    catch
                        error:Reason ->
                            ?ERROR(Reason, 'before', Before, 
                                   [{type, Type},
                                    {context, Context},
                                    {form, Form}])
                    end,
                Form2 =
                    case erl_syntax:subtrees(Form1) of
                        [] ->
                            Form1;
                        List ->
                            NewList =
                                transform(
                                  List, Before, After, Context1),
                            erl_syntax:update_tree(Form, NewList)
                    end,
                Type2 = erl_syntax:type(Form2),
                try After(Type2, Form2, Context1)
                catch
                    error:Reason2 ->
                        ?ERROR(Reason2, 'after', After, 
                               [{type, Type2},
                                {context, Context1},
                                {form, Form2}])
                end
        end,
    F2 = fun(List) when is_list(List) ->
                 map(F1, List);
            (Form) ->
                 F1(Form)
         end,
    map(F2, Forms).

%%% Slightly modified version of lists:mapfoldl/3
%%% Here, F/2 is able to insert forms before and after the form
%%% in question. The inserted forms are not transformed afterwards.
map(F, [Hd|Tail]) ->
    {Before, Res, After} =
        case F(Hd) of
            {Be, _, Af} = Result when is_list(Be), is_list(Af) ->
                Result;
            R1 ->
                {[], R1, []}
        end,
    Rs = map(F, Tail),
    Before ++ [Res| After ++ Rs];
map(F, []) when is_function(F, 1) -> [].

rpt_error(Reason, BeforeOrAfter, Fun, Info) ->
    Fmt = lists:flatten(
            ["*** ERROR in parse_transform function:~n"
             "*** Reason     = ~p~n"
             "*** applying ~w fun (~p)~n",
             "*** function info: ~p~n",
            ["*** ~10w = ~p~n" || _ <- Info]]),
    Args = [Reason, BeforeOrAfter, Fun, erlang:fun_info(Fun) | 
            lists:foldr(
              fun({K,V}, Acc) ->
                      [K, V | Acc]
              end, [], Info)],
    io:format(Fmt, Args).

should_i_bind(Tree) ->
    erl_syntax_lib:fold(
      fun(T, Flag) ->
              lists:member(bind_state, get_ann(T)) or Flag
      end, false, Tree).

v(V) -> erl_syntax:variable(V).



