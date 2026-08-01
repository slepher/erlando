%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_cont_instance).

-compile({parse_transform, cut}).

-include("do.hrl").
-include("erlando_instance.hrl").

-erlando_instance(
   #{types => [reader_t, writer_t, state_t, maybe_t, error_t, except_t, list_t],
     capability => monad_cont,
     implementation =>
         {dispatch,
          #{reader_t => #{callCC => {reader_t_call_cc, 2}},
            writer_t => #{callCC => {writer_t_call_cc, 2}},
            state_t => #{callCC => {state_t_call_cc, 2}},
            maybe_t => #{callCC => {maybe_t_call_cc, 2}},
            error_t => #{callCC => {error_t_call_cc, 2}},
            except_t => #{callCC => {except_t_call_cc, 2}},
            list_t => #{callCC => {list_t_call_cc, 2}}}}}).

%%%===================================================================
%%% API
%%%===================================================================
reader_t_call_cc(F, {reader_t, MonadCont}) ->
    CallCC = monad_cont:callCC(_, MonadCont),
    reader_t:reader_t(
      fun(R) ->
              CallCC(
                fun(CC) ->
                        reader_t:run(
                          F(fun(A) -> reader_t:reader_t(fun(_) -> CC(A) end) end),
                          R)
                end)
      end).

writer_t_call_cc(F, {writer_t, MonadCont}) ->
    CallCC = monad_cont:callCC(_, MonadCont),
    writer_t:writer_t(
      CallCC(
        fun(CC) ->
                writer_t:run_writer_t(
                  F(fun(A) -> writer_t:writer_t(CC({A, monoid:mempty()})) end))
        end)).

state_t_call_cc(F, {state_t, MonadCont}) ->
    CallCC = monad_cont:callCC(_, MonadCont),
    state_t:state_t(
      fun(S) ->
              CallCC(
                fun(CC) ->
                        state_t:run(
                          F(fun(A) -> state_t:state_t(fun(_) -> CC({A, S}) end) end),
                          S)
                end)
      end).

maybe_t_call_cc(F, {maybe_t, MonadCont}) ->
    CallCC = monad_cont:callCC(_, MonadCont),
    maybe_t:maybe_t(
      CallCC(
        fun(CC) ->
                maybe_t:run_maybe_t(F(fun(A) -> maybe_t:maybe_t(CC({just, A})) end))
        end)).

error_t_call_cc(F, {error_t, MonadCont}) ->
    CallCC = monad_cont:callCC(_, MonadCont),
    error_t:error_t(
      CallCC(
        fun(CC) ->
                error_t:run_error_t(F(fun(A) -> error_t:error_t(CC({right, A})) end))
        end)).

except_t_call_cc(F, {except_t, MonadCont}) ->
    CallCC = monad_cont:callCC(_, MonadCont),
    except_t:except_t(
      CallCC(
        fun(CC) ->
                except_t:run_except_t(F(fun(A) -> except_t:except_t(CC({right, A})) end))
        end)).

list_t_call_cc(F, {list_t, MonadCont}) ->
    CallCC = monad_cont:callCC(_, MonadCont),
    list_t:list_t(
      CallCC(
        fun(CC) ->
                list_t:run_list_t(
                  F(
                    fun(A) ->
                            list_t:list_t(CC({cons, A, monad:return(nil, MonadCont)}))
                    end))
        end)).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
