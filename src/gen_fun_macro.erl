%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 21 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_fun_macro).

-include_lib("astranaut/include/quote.hrl").

%% API
-export([gen_fun/2]).

%%%===================================================================
%%% API
%%%===================================================================
gen_fun(Opts,Attrs) ->
    generate_forms(Opts, Attrs).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_forms(Opts, #{module := Module, line := Line, erlando_type := ErlandoTypes}) ->
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
                                end, Functions),
                          UFunctions ++ Acc
                  end, [], PatternsGroup);
            source ->
                Functions
        end,
    lists:foldl(
      fun(Pattrens, Acc) ->
              Forms = 
                  lists:map(
                    fun({FName, Arity}) ->
                            gen_function(
                              Remote, FName, Arity, Line, Pattrens, NNExtraArgs, ExtraCall)
                    end, NFunctions),
              Forms ++ Acc
      end, [], PatternsGroup).

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
      end, [], Behaviours).

update_args(_Remote, Args) when is_list(Args) ->
    Args;
update_args(Remote, Arg) ->
    [{Remote, Arg}].

gen_function(Module, FName, Arity, Line, ExtraPatterns, ExtraArgs, ExtraCall) ->
    NArity = Arity - length(ExtraArgs), 
    UArity = NArity + length(ExtraPatterns),
    Patterns = 
        lists:map(
          fun(N) ->
                  {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
          end, lists:seq(1, NArity)),
    BPatterns = 
        lists:map(
          fun(BehaviourPattern) ->
                  astranaut:abstract(BehaviourPattern, Line)
          end, ExtraPatterns),
    GPatterns = Patterns ++ BPatterns,
    GCall = [gen_call(Module, FName, NArity, Line, ExtraArgs, ExtraCall)],
    {function, Line, FName, UArity, 
     [{clause, Line, GPatterns, [], GCall}]}.

gen_call(Module, FName, Arity, Line, ExtraArgs, {Remote, Function}) ->
    {call, Line, {remote, Line, {atom, Line, Remote}, {atom, Line, Function}},
     [gen_call(Module, FName, Arity, Line, ExtraArgs, undefined)]};
gen_call(Module, FName, Arity, Line, ExtraArgs, undefined) ->
    Call = 
        case Module of
            undefined ->
                {atom, Line, FName};
            Remote ->
                {remote, Line, {atom, Line, Remote}, {atom, Line, FName}}
        end,
    Args = 
        lists:map(
          fun(ExtraArg) ->
                 astranaut:abstract(ExtraArg, Line)
          end, ExtraArgs),
    {call, Line, Call,
     lists:map(
       fun(N) ->
               {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
       end, lists:seq(1, Arity)) ++ Args
    }.


type(ErlandoTypes) ->
    case ErlandoTypes of
        [{Type, _Patterns}] ->
            Type;
        [Type] ->
            Type;
        [] ->
            undefined
    end.
