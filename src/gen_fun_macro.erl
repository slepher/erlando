%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 21 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_fun_macro).

-include_lib("astranaut/include/macro.hrl").

-export_macro([{gen_fun/2, [{inject_attrs, erlando_type}, {as_attr, gen_fun}]}]).
-export([generate_forms/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
gen_fun(Opts, Attrs) ->
    try
        generate_forms(Opts, Attrs)
    catch
        error:Reason ->
            {error, Reason}
    end.

format_error(undefined_type) ->
    io_lib:format("inner_type is specified but the erlando type is undefined", []).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_forms(Opts, #{module := Module, pos := Line, erlando_type := ErlandoTypes}) ->
    Type = type(ErlandoTypes),
    Remote = maps:get(remote, Opts, Module),
    ExtraPatterns = maps:get(patterns, Opts, []),
    ExtraPatternsGroup = maps:get(patterns_group, Opts, [ExtraPatterns]),
    ExtraArgs = maps:get(args, Opts, []),
    ExtraCall = maps:get(extra_call, Opts, undefined),
    {Functions, DefaultArityMode} = get_functions_and_arity_mode(Opts),
    ArityMode = maps:get(am, Opts, DefaultArityMode),
    {PatternsGroup, NExtraArgs} =
        case maps:find(inner_type, Opts) of
            {ok, InnerType} ->
                case Type of
                    undefined ->
                        exit(undefined_type);
                    Type ->
                        {[[], [Type]], InnerType}
                end;
            error ->
                {ExtraPatternsGroup, ExtraArgs}
        end,
    NNExtraArgs = update_args(Remote, NExtraArgs),
    NFunctions =
        case ArityMode of
            target ->
                lists:foldl(
                    fun(Patterns, Acc) ->
                        ArityDiff = length(NNExtraArgs) - length(Patterns),
                        UFunctions =
                            lists:map(
                                fun({FName, Arity}) ->
                                    {FName, Arity + ArityDiff}
                                end,
                                Functions
                            ),
                        UFunctions ++ Acc
                    end,
                    [],
                    PatternsGroup
                );
            source ->
                Functions
        end,
    GeneratedForms = lists:foldl(
        fun(Pattrens, Acc) ->
            Forms =
                lists:map(
                    fun({FName, Arity}) ->
                        gen_function(
                            Module, Remote, FName, Arity, Line, Pattrens, NNExtraArgs, ExtraCall
                        )
                    end,
                    NFunctions
                ),
            Forms ++ Acc
        end,
        [],
        PatternsGroup
    ),
    [{attribute, Line, gen_fun_meta, {1, Opts}} | GeneratedForms].

get_functions_and_arity_mode(Opts) ->
    case maps:find(functions, Opts) of
        {ok, Functions} ->
            {Functions, target};
        error ->
            case maps:find(sfunctions, Opts) of
                {ok, BFunctions} ->
                    {BFunctions, source};
                error ->
                    case maps:find(behaviours, Opts) of
                        {ok, Behaviours} ->
                            {behaviour_functions(Behaviours), source};
                        error ->
                            case maps:find(tbehaviours, Opts) of
                                {ok, Behaviours} ->
                                    {behaviour_functions(Behaviours), target};
                                error ->
                                    {[], source}
                            end
                    end
            end
    end.

behaviour_functions(Behaviours) ->
    lists:foldl(
        fun(Behaviour, Acc0) ->
            Callbacks = Behaviour:behaviour_info(callbacks),
            Callbacks ++ Acc0
        end,
        [],
        Behaviours
    ).

update_args(_Remote, Args) when is_list(Args) ->
    Args;
update_args(Remote, Arg) ->
    [{Remote, Arg}].

gen_function(Module, Remote, FName, Arity, Line, ExtraPatterns, ExtraArgs, ExtraCall) ->
    NArity = Arity - length(ExtraArgs),
    UArity = NArity + length(ExtraPatterns),
    Patterns =
        lists:map(
            fun(N) ->
                {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
            end,
            lists:seq(1, NArity)
        ),
    BPatterns =
        lists:map(
            fun(BehaviourPattern) ->
                astranaut_lib:abstract_form(BehaviourPattern, Line)
            end,
            ExtraPatterns
        ),
    GPatterns = Patterns ++ BPatterns,
    LenCurrent = length(GPatterns),
    LenRemote = length(ExtraArgs) + NArity,
    FName1 =
        case Remote of
            Remote when Remote == Module ->
                if
                    LenCurrent == LenRemote ->
                        '__original__';
                    true ->
                        FName
                end;
            _ ->
                FName
        end,
    GCall = [gen_call(Module, Remote, FName, NArity, Line, ExtraArgs, ExtraCall)],
    FName1 =
        case Remote of
            Remote when Remote == Module ->
                if
                    LenCurrent == LenRemote ->
                        '__original__';
                    true ->
                        FName
                end;
            _ ->
                FName
        end,
    case FName1 of
        '__original__' ->
            UPatterns =
                lists:map(
                    fun(N) ->
                        {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
                    end,
                    lists:seq(1, UArity)
                ),
            GCall1 = [gen_call(Module, Remote, FName1, UArity, Line, [], undefined)],
            [
                {attribute, Line, export, [{FName, UArity}]},
                {function, Line, FName, UArity, [
                    {clause, Line, GPatterns, [], GCall}, {clause, Line, UPatterns, [], GCall1}
                ]}
            ];
        _ ->
            [
                {attribute, Line, export, [{FName, UArity}]},
                {function, Line, FName, UArity, [{clause, Line, GPatterns, [], GCall}]}
            ]
    end.

gen_call(Module, Remote, FName, Arity, Line, ExtraArgs, {RemoteF, Function}) ->
    {call, Line, {remote, Line, {atom, Line, RemoteF}, {atom, Line, Function}}, [
        gen_call(Module, Remote, FName, Arity, Line, ExtraArgs, undefined)
    ]};
gen_call(Module, Remote, FName, Arity, Line, ExtraArgs, undefined) ->
    Call =
        case Remote of
            Module ->
                {atom, Line, FName};
            Remote ->
                {remote, Line, {atom, Line, Remote}, {atom, Line, FName}}
        end,
    Args =
        lists:map(
            fun(ExtraArg) ->
                astranaut_lib:abstract_form(ExtraArg, Line)
            end,
            ExtraArgs
        ),
    {call, Line, Call,
        lists:map(
            fun(N) ->
                {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
            end,
            lists:seq(1, Arity)
        ) ++ Args}.

type(ErlandoTypes) ->
    case ErlandoTypes of
        [{Type, _Patterns}] ->
            Type;
        [Type] ->
            Type;
        [] ->
            undefined
    end.
