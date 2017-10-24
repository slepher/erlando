%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erlando_typeclass).

-behaviour(gen_server).

%% API
-export([module/2]).
-export([register_application/1, register_modules/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {behaviour_modules = maps:new()}).

%%%===================================================================
%%% API
%%%===================================================================

module(Type, Behaviour) ->
    typeclass:module(Type, Behaviour).

register_application(Application) ->
    case application:get_key(Application, modules) of
        {ok, Modules} ->
            register_modules(Modules);
        undefined ->
            {error, undefined}
    end.

register_modules(Modules) ->
    gen_server:call(?SERVER, {register_modules, Modules}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    load_module(maps:new()),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({register_modules, Modules}, _From, #state{behaviour_modules = BehaviourModules} = State) ->
    NBehaviourModules = 
        lists:foldl(
          fun(Module, Acc0) ->
                  Behaviours = behaviours(Module),
                  case lists:member(type, Behaviours) of
                      true ->
                          Type = Module:type(),
                          lists:foldl(
                            fun(Behaviour, Acc1) ->
                                    maps:put({Type, Behaviour}, Module, Acc1)
                            end, Acc0, Behaviours -- [type]);
                      false ->
                          Acc0
                  end
          end, BehaviourModules, Modules),
    load_module(NBehaviourModules),
    {reply, ok, State#state{behaviour_modules = NBehaviourModules}};

handle_call({module, Type, Behaviour}, _From, #state{behaviour_modules = BehaviourModules} = State) ->
    Reply = 
        case maps:find({Type, Behaviour}, BehaviourModules) of
            {ok, Module} ->
                {just, Module};
            error ->
                nothing
        end,
    {reply, Reply, State}; 

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_module(BehaviourModules) ->
    Module = generate_module(BehaviourModules),
    {ok, Mod, Bin} = compile:forms(Module),
    code:load_binary(Mod, [], Bin).

generate_module(BehaviourModules) ->
    TypeclassModule = {attribute,0,module,typeclass},
    Export = {attribute,0,export,[{module,2}]},
    Clauses = 
        maps:fold(
          fun({Type, Behaviour}, Module, Acc) ->
                  [module_clause(0, Type, Behaviour, Module)|Acc]
          end, [], BehaviourModules),
    LastClause = {clause, 0, [{var, 0, 'A'}, {var, 0, 'B'}], [], 
                  [{call, 0, {atom, 0, exit}, 
                    [{tuple, 0, [{atom, 0, unregisted_module}, {tuple, 0, [{var, 0, 'A'}, {var, 0, 'B'}]}]}]}]},
    Function = {function, 0, module, 2, lists:reverse([LastClause|Clauses])},
    [TypeclassModule, Export, Function].

module_clause(Line, Type, Behaviour, Module) ->
    {clause, 1, [{atom, Line, Type}, {atom, Line, Behaviour}], [],
     [{atom, Line, Module}]}.

behaviours(Module) ->
    Attributes = Module:module_info(attributes),
    lists:foldl(
      fun({behaviour, Behaviours}, Acc) ->
              Behaviours ++ Acc;
         (_Other, Acc) ->
              Acc
      end, [], Attributes).
