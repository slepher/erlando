%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 13 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(do_runtime).

%% API
-export(['>>='/3, return/2, fail/2]).

%%%===================================================================
%%% API
%%%===================================================================
'>>='(MA, KMB, Monad) ->
    NMonad = convert_monad(Monad),
    monad:'>>='(MA, KMB, NMonad).

return(A, Monad) ->
    NMonad = convert_monad(Monad),
    monad:return(A, NMonad).

fail(E, Monad) ->
    NMonad = convert_monad(Monad),
    monad_fail:fail(E, NMonad).

convert_monad(Monad) ->
    case erlando:version() of
        classic ->
            Monad;
        _ ->
            do_convert_monad(Monad)
    end.

do_convert_monad(error_m) ->
    error;
do_convert_monad(maybe_m) ->
    maybe;
do_convert_monad(list_m) ->
    list;
do_convert_monad(identity_m) ->
    identity;
do_convert_monad({Trans, Monad}) ->
    {Trans, convert_monad(Monad)};
do_convert_monad(Monad) ->
    Monad.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
