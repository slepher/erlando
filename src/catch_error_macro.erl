%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 21 Oct 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(catch_error_macro).

-include_lib("astranaut/include/quote.hrl").
-include_lib("astranaut/include/macro.hrl").

%% API
-export([cat_error/1, cat_error/2]).
-export([format_error/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
cat_error(Ast) ->
    do_cat_error(Ast, #{}).

cat_error(Ast, quote = _A@Label) ->
    do_cat_error(Ast, #{label => Label});
cat_error(Ast, OptsAst) ->
    Opts = astranaut:ast_to_options(OptsAst),
    do_cat_error(Ast, Opts).

format_error(Error, #{label := Label}, _AstString) ->
    {Label, Error};
format_error(Error, #{}, AstString) ->
    {execute_failed, AstString, Error}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_cat_error(Ast, Opts) ->
    Opts1 = astranaut:abstract(Opts),
    String = astranaut:abstract(list_to_binary(astranaut:to_string(Ast))),
    quote(monad_error:catch_error(
            unquote(Ast),
            fun(Error) ->
                    Error1 = catch_error_macro:format_error(Error, unquote(Opts1), unquote(String)),
                    monad_error:throw_error(Error1)
            end)).
