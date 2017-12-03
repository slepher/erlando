%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(function_generator).

%% API
-export([parse_transform/2]).
%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Opts) ->
    [Module] = ast_traverse:attributes(module, Forms),
    MFs = ast_traverse:attributes_with_line(gen_fun, Forms),
    Type = type(Forms),
    lists:foldl(
      fun({Line, Transform}, FormsAcc) ->
              GForms = generate_forms(Module, Type, Line, Transform),
              insert_gforms(GForms, FormsAcc)
          end, Forms, MFs).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%%%===================================================================
%%% Internal functions
%%%===================================================================
insert_gforms(BForms, Forms) ->
    lists:foldl(fun insert_gform/2, Forms, BForms).

insert_gform(BForm, Forms) ->
    insert_gform(BForm, Forms, []).

insert_gform({function, _Line, FName, Arity, Clauses}, [{function, Line, FName, Arity, FClauses}|T], Heads) ->
    lists:reverse(Heads) ++ [{function, Line, FName, Arity, Clauses ++ FClauses}|T];
insert_gform({function, Line, FName, Arity, _Clauses} = BForm, [{eof, _ELine} = EOF|T], Heads) ->
    Exports = [{Line, [{FName, Arity}]}],
    GExports = export_funs(Exports),
    NForm = lists:reverse(Heads) ++ [BForm, EOF|T],
    insert_exports(GExports, NForm, []);
insert_gform(BForm, [H|T], Heads) ->
    insert_gform(BForm, T, [H|Heads]).

export_funs(FWithLines) ->
    [{attribute,Line,export,[{F,A} || {F,A} <- Functions]} || {Line, Functions} <- FWithLines].

insert_exports(Exports, [{attribute,_Line,module,_Mod} = Module|T], Acc) ->
    lists:reverse(Acc) ++ [Module|Exports] ++ T;
insert_exports(Exports, [Form|Forms], Acc) ->
    insert_exports(Exports, Forms, [Form|Acc]).

generate_forms(Module, Type, Line, Opts) ->
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

type(Forms) ->
    case lists:flatten(ast_traverse:attributes(erlando_type, Forms)) of
        [{Type, _Patterns}] ->
            Type;
        [Type] ->
            Type;
        _ ->
            undefined
    end.

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
                  ast_traverse:from_value(Line, BehaviourPattern)
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
                 ast_traverse:from_value(Line, ExtraArg)
          end, ExtraArgs),
    {call, Line, Call,
     lists:map(
       fun(N) ->
               {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
       end, lists:seq(1, Arity)) ++ Args
    }.