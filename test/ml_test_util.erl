%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(ml_test_util).

-compile({parse_transform, do}).

%% API
-export([left_identity/2, right_identity/1]).
-export([associativity1/3, associativity2/3, associativity3/3]).

%%%===================================================================
%%% API
%%%===================================================================

left_identity(X, F) ->
    do([monad ||
           A <- F(X),
           return(A)
       ]).

right_identity(M) ->
    do([monad ||
           A <- M,               
           return(A)
       ]).

associativity1(M, F, G) ->
    do([monad ||
           A <- M,
           do([monad ||
                  B <- F(A),
                  G(B)
              ])
       ]).

associativity2(M, F, G) ->
    do([monad ||
           B <- do([monad ||
                       A <- M,
                       F(A)
                   ]),
           G(B)
       ]).

associativity3(M, F, G) ->
    do([monad || 
           A <- M,
           B <- F(A),
           G(B)
       ]).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
