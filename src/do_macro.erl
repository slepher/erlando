%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 21 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(do_macro).

-include_lib("astranaut/include/quote.hrl").

%% API
-export([do/1]).
-export([format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
do({lc, Line, {MonadType, _Line3, _MonadModule} = Monad, Comprehensions}) ->
    case lists:member(MonadType, [atom, var, tuple]) of
        true ->
            case do_comprehensions(Comprehensions, Monad) of
                Expressions when is_list(Expressions) ->
                    quote(
                      (fun() ->
                               unquote_splicing(Expressions)
                       end)(), Line);
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, expected_monad_type}
    end;
do(_Ast) ->
    {error, expected_list_comprehension}.

format_error(non_empty_do) ->
    "A 'do' construct cannot be empty";
format_error(non_last_expression) ->
    "The last statement in a 'do' construct must be an expression";
format_error(Reason) ->
    astranaut_traverse:format_error(Reason).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%  'do' syntax transformation:
do_comprehensions([], _Monad) ->
    {error, non_empty_do};
do_comprehensions([{GenerateOrMatch, _Line, _Pattern, _Expr}], _Monad)
  when GenerateOrMatch =:= generate orelse GenerateOrMatch =:= match ->
    {error, non_last_expression};
do_comprehensions([{generate, Line, Pattern, Expr} | Exprs], Monad) ->
    %% "Pattern <- Expr, Tail" where Pattern is a simple variable
    %% is transformed to
    %% "monad:'>>='(Monad, Expr, fun (Pattern) -> Tail')"
    %% without a fail to match clause
    NExpr = update_expression(Expr, Monad),
    [quote(monad:'>>='(unquote(NExpr), unquote(bind_expression(Line, Pattern, Exprs, Monad)), unquote(Monad)), Line)];
do_comprehensions([Expr], Monad) ->
    %% Don't do '>>' chaining on the last elem
    NExpr = update_expression(Expr, Monad),
    [NExpr]; 
do_comprehensions([{match, _Line, _Pattern, _Expr} = Expr | Exprs], Monad) ->
    %% Handles 'let binding' in do expression a-la Haskell
    NExpr = update_expression(Expr, Monad),
    [NExpr|do_comprehensions(Exprs, Monad)];
do_comprehensions([Expr | Exprs], Monad) ->
    %% "Expr, Tail" is transformed to "monad:'>>='(Monad, Expr, fun (_) -> Tail')"
    Line = erl_syntax:get_pos(Expr),
    case do_comprehensions(Exprs, Monad) of
        RestExprs when is_list(RestExprs) ->
            NExpr = update_expression(Expr, Monad),
            [quote(monad:'>>='(unquote(NExpr), fun(_) -> unquote_splicing(RestExprs) end, unquote(Monad)), Line)];
        {error, Reason} ->
            {error, Reason}
    end.

update_expression(Expression, Monad) ->
    astranaut_traverse:map(
      fun({call, Line, {atom, _Line1, fail}, [Arg]}, _Attr) ->
              %% 'return' calls of a particular form:
              %% return(Argument), and
              %% Transformed to:
              %% "monad:return(Argument, Monad)" in monadic context
              quote(monad_fail:fail(unquote(Arg), unquote(Monad)), Line);
         ({call, Line, {atom, _Line1, return}, [Arg]}, _Attr) ->
              %% 'fail' calls of a particular form:
              %% fail(Argument)
              %% Transformed to:
              %% 'monad_fail:fail(Argument, Monad)" in monadic context
              quote(monad:return(unquote(Arg), unquote(Monad)), Line);
         (Node, _Attr) ->
              Node
      end, Expression).

bind_expression(Line, {var, _Line, _Var} = Pattern, Exprs, Monad) ->
    quote(
      fun(unquote = Pattern) ->
              unquote_splicing(do_comprehensions(Exprs, Monad))
      end, Line);
bind_expression(Line, Pattern, Exprs, Monad) ->
    LineExpr = astranaut:abstract(Line, Line),
    String = astranaut:abstract(astranaut:to_string(Pattern), Line),
    quote(
      fun(unquote = Pattern) ->
              unquote_splicing(do_comprehensions(Exprs, Monad));
         (Var) ->
              monad_fail:fail({monad_badmatch, Var, unquote(LineExpr), unquote(String)})
      end, Line).
