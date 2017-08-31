%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 30 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(another_cut).

%% API
-export([parse_transform/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Opts) ->
    {ok, {NForms, _State}} = ast_traverse:traverse(fun(Type, Node, State) -> {ok, {walk(Type, Node), State}} end, ok, Forms),
    NForms.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk(pre, {cons, Line, _H0, T0} = Cons) ->
    case find_cons_cut_vars([Cons], T0) of
        {[], _H1T1} ->
            Cons;
        {Pattern, NCons} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [NCons]}]}}
    end;
walk(pre, {lc, Line, E0, Qs0} = Lc) ->
    %% Note that it is nonsensical to allow a cut on E0, as in all
    %% useful cases, it is defined by some expression of Qs0. Cuts are
    %% allowed only on generators of Qs0.
    Qs = find_comprehension_cut_vars(Qs0),
    case Qs of
        {[], _Qs1} ->
            Lc;
        {Pattern, Qs1} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{lc, Line, E0, Qs1}]}]}}
    end;
walk(pre, {bc, Line, E0, Qs0} = Bc) ->
    %% Notes for {lc,...} above apply here too.
    Qs = find_comprehension_cut_vars(Qs0),
    case Qs of
        {[], _Qs1} ->
            Bc;
        {Pattern, Qs1} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{bc, Line, E0, Qs1}]}]}}
    end;
walk(pre, {tuple, Line, Es0} = Tuple) ->
    case find_cut_vars(Es0) of
        {[],     _Es1} ->
            Tuple;
        {Pattern, Es1} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{tuple, Line, Es1}]}]}}
    end;
%% OTP 17.0: EEP 443: Map construction
walk(pre, {map, Line, Fields0} = Map) ->
    case find_map_cut_vars(Fields0) of
        {[],     _Fields1} ->
            Map;
        {Pattern, Fields1} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{map, Line, Fields1}]}]}}
    end;
walk(pre, {record, Line, Name, Inits0} = Record) ->
    case find_record_cut_vars(Inits0) of
        {[],     _Inits0} ->
            Record;
        {Pattern, Inits1} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{record, Line, Name, Inits1}]}]}}
    end;
walk(pre, {record_field, Line, Rec0, Name, Field0} = Record) ->
    case find_cut_vars([Rec0]) of
        {[], _Rec1} ->
            Record;
        {Pattern, [Rec1]} ->
            {'fun', Line, {clauses,
                           [{clause, Line, Pattern, [],
                             [{record_field, Line, Rec1, Name, Field0}]}]}}
    end;
walk(pre, {record, Line, Rec0, Name, Upds0} = Record) ->
    Rec = find_cut_vars([Rec0]),
    Upds = find_record_cut_vars(Upds0),
    case {Rec, Upds} of
        {{[], _Rec2}, {[], _Upds2}} ->
            Record;
        {{Pattern1, [Rec1]}, {Pattern2, Upds1}} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern1++Pattern2, [],
                                      [{record, Line, Rec1, Name, Upds1}]}]}}
    end;
walk(pre, {'case', Line, E0, Cs0} = Case) ->
    case find_cut_vars([E0]) of
        {[], _E2} ->
            Case;
        {Pattern, [E1]} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{'case', Line, E1, Cs0}]}]}}
    end;
walk(pre, {call, Line, F0, As0} = Call) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    %%
    %% If F0 is a remote call then we want to allow cuts, but we don't
    %% want F0 to end up forming a separate function. Thus we have
    %% find_call_cut_vars and we brings cuts from within that up here.
    F = find_call_cut_vars(F0),
    As = find_cut_vars(As0),
    case {F, As} of
        {{[], _F2}, {[], _As2}} ->
            Call;
        {{Pattern1, [F1]}, {Pattern2, As1}} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern1++Pattern2, [],
                                      [{call, Line, F1, As1}]}]}}
    end;
walk(pre, {bin, Line, Fs0} = Bin) ->
    case find_binary_cut_vars(Fs0) of
        {[], _Fs1} ->
            Bin;
        {Pattern, Fs1} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{bin, Line, Fs1}]}]}}
    end;
walk(pre, {op, Line, Op, L0, R0} = Op) ->
    case find_cut_vars([L0, R0]) of
        {[], _L1R1} ->
            Op;
        {Pattern, [L1, R1]} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{op, Line, Op, L1, R1}]}]}}
    end;
walk(_Type, Node) ->
    Node.

%% Turns out you can't abstract out binary types:
%% 1> X = binary, Y = fun (Z) -> <<Z/X>> end.
%% * 1: syntax error before: X
%% I didn't know that. I still support that in cuts though you can't
%% use it on the grounds that Erlang might fix this at some later
%% point.
find_binary_cut_vars(BinFields) ->
    cut_vars(
      fun ({bin_element, _Line, Var, Size, Type}) ->
              [V || V = {var, _Line1, '_'} <- [Var, Size, Type]];
          (_) ->
              []
      end,
      fun ({bin_element, Line, Var, Size, Type}, Vars) ->
              {[Var1, Size1, Type1], []} =
                  lists:foldr(
                    fun ({var, _Line, '_'}, {Res, [V|Vs]}) -> {[V|Res], Vs};
                        (V,                 {Res, Vs})     -> {[V|Res], Vs}
                    end, {[], Vars}, [Var, Size, Type]),
              {bin_element, Line, Var1, Size1, Type1}
      end,
      BinFields).

find_map_cut_vars(MapFields) ->
    cut_vars(
      fun ({map_field_assoc, _Line, {var, _Line1, '_'} = ExpK, {var, _Line2, '_'} = ExpV}) -> [ExpK, ExpV];
          ({map_field_assoc, _Line, {var, _Line1, '_'} = ExpK,                     _ExpV}) -> [ExpK];
          ({map_field_assoc, _Line,                     _ExpK, {var, _Line1, '_'} = ExpV}) -> [ExpV];
          ({map_field_assoc, _Line,                     _ExpK,                     _ExpV}) -> [];
          ({map_field_exact, _Line, {var, _Line1, '_'} = ExpK, {var, _Line2, '_'} = ExpV}) -> [ExpK, ExpV];
          ({map_field_exact, _Line, {var, _Line1, '_'} = ExpK,                     _ExpV}) -> [ExpK];
          ({map_field_exact, _Line,                     _ExpK, {var, _Line1, '_'} = ExpV}) -> [ExpV];
          ({map_field_exact, _Line,                     _ExpK,                     _ExpV}) -> [];
          (_)                                                                              -> []
      end,
      fun ({map_field_assoc, Line, _ExpK             , _ExpV             }, [ExpK, ExpV]) -> {map_field_assoc, Line, ExpK, ExpV};
          ({map_field_assoc, Line, {var, _Line1, '_'},  ExpV             }, [ExpK]      ) -> {map_field_assoc, Line, ExpK, ExpV};
          ({map_field_assoc, Line, ExpK              , {var, _Line2, '_'}}, [ExpV]      ) -> {map_field_assoc, Line, ExpK, ExpV};
          ({map_field_exact, Line, _ExpK             , _ExpV             }, [ExpK, ExpV]) -> {map_field_assoc, Line, ExpK, ExpV};
          ({map_field_exact, Line, {var, _Line1, '_'},  ExpV             }, [ExpK]      ) -> {map_field_assoc, Line, ExpK, ExpV};
          ({map_field_exact, Line, ExpK              , {var, _Line2, '_'}}, [ExpV]      ) -> {map_field_assoc, Line, ExpK, ExpV}
      end,
      MapFields).

find_record_cut_vars(RecFields) ->
    cut_vars(
      fun ({record_field, _Line, _FName, {var, _Line1, '_'} = Var}) -> [Var];
          (_)                                                       -> []
      end,
      fun ({record_field, Line, FName, _Var}, [Var]) ->
              {record_field, Line, FName, Var}
      end,
      RecFields).

find_comprehension_cut_vars(Qs) ->
    cut_vars(
      fun ({generate,   _Line, _P0, {var, _Line1, '_'} = Var}) -> [Var];
          ({generate,   _Line, _P0, _E0})                      -> [];
          ({b_generate, _Line, _P0, {var, _Line1, '_'} = Var}) -> [Var];
          ({b_generate, _Line, _P0, _E0})                      -> [];
          (_)                                                  -> []
      end,
      fun ({generate,   Line, P0, _Var}, [Var]) -> {generate, Line, P0, Var};
          ({b_generate, Line, P0, _Var}, [Var]) -> {b_generate, Line, P0, Var}
      end,
      Qs).

find_call_cut_vars(F) ->
    cut_vars(
      fun ({remote, _Line, M0, F0}) -> [V || V = {var, _Line1, '_'} <- [M0,F0]];
          ({var, _Line, '_'} = Var) -> [Var];
          (_)                       -> []
      end,
      fun ({remote, Line, M0, F0}, Vars) ->
              {[M1, F1], []} =
                  lists:foldr(
                    fun ({var, _Line, '_'}, {Res, [V|Vs]}) -> {[V|Res], Vs};
                        (V,                 {Res, Vs})     -> {[V|Res], Vs}
                    end, {[], Vars}, [M0, F0]),
              {remote, Line, M1, F1};
          ({var, _Line, _Var}, [Var]) -> Var
      end,
      [F]).

find_cons_cut_vars(HeadsRev, {cons, _Line, _Head, Tail} = Cons) ->
    find_cons_cut_vars([Cons | HeadsRev], Tail);
find_cons_cut_vars(HeadsRev, Other) ->
    Heads = lists:reverse([Other|HeadsRev]),
    {Pattern, Heads1} =
        cut_vars(
          fun ({cons, _Line, {var, _Line1, '_'} = Var, _Tail}) -> [Var];
              ({var, _Line, '_'} = Var)                        -> [Var];
              (_)                                              -> []
          end,
          fun ({cons, Line, {var, _Line1, '_'}, Tail}, [Var]) ->
                  {cons, Line, Var, Tail};
              ({var, _Line, '_'}, [Var]) ->
                  Var
          end,
          Heads),
    {Pattern,
     lists:foldr(
       fun ({cons, Line, Head, _Tail}, Tail) -> {cons, Line, Head, Tail};
           (Tail, undefined)                 -> Tail
       end, undefined, Heads1)}.

find_cut_vars(As) ->
    cut_vars(fun ({var, _Line, '_'} = Var) -> [Var];
                 (_)                       -> []
             end,
             fun (_, [{var, _Line, _Var} = Var]) -> Var end,
             As).

cut_vars(TestFun, CombFun, AstFrag) ->
    cut_vars(TestFun, CombFun, AstFrag, [], []).

cut_vars(_TestFun, _CombFun, [], Pattern, AstAcc) ->
    {lists:reverse(Pattern), lists:reverse(AstAcc)};
cut_vars(TestFun, CombFun, [Frag|AstFrags], Pattern, AstAcc) ->
    case TestFun(Frag) of
        [] ->
            cut_vars(TestFun, CombFun, AstFrags, Pattern, [Frag|AstAcc]);
        Vars ->
            Vars1 = [{var, Line, make_var_name()} || {var, Line, _} <- Vars],
            Frag1 = CombFun(Frag, Vars1),
            cut_vars(TestFun, CombFun, AstFrags,
                     Vars1 ++ Pattern, [Frag1|AstAcc])
    end.

make_var_name() ->
    VarCount = get(var_count),
    put(var_count, VarCount+1),
    list_to_atom("__cut_" ++ integer_to_list(VarCount)).
