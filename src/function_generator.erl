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
    Opts =  [{attrs, [erlando_type]}, {as_attr, gen_fun}, auto_export, {merge_function, head}],
    astranaut_macro:transform_macro(gen_fun_macro, gen_fun, 2, Opts, Forms).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%%%===================================================================
%%% Internal functions
%%%===================================================================
