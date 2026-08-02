-module(erlando_instance_macro).

-include_lib("astranaut/include/macro.hrl").

-export_macro([{erlando_instance/2,
                [{inject_attrs, true}, {as_attr, erlando_instance}]}]).

erlando_instance(Spec, #{module := Module, pos := Line} = Attrs) when is_map(Spec) ->
    ok = validate_spec(Spec),
    TypeDeclarations = type_declarations(Spec),
    TypeNames0 = [type_name(Type) || Type <- TypeDeclarations],
    TypeNames = unique_values(types, TypeNames0),
    Capabilities = unique_capabilities(capabilities(Spec)),
    Instances =
        [instance_metadata(Type, Typeclass, Adapter)
         || Type <- TypeNames,
            {Typeclass, Adapter} <- Capabilities],
    Metadata = #{module => Module,
                 types => TypeNames,
                 instances => Instances},
    TypeForms =
        [{attribute, Line, erlando_type, Type} || Type <- TypeDeclarations],
    BehaviourForms =
        [{attribute, Line, behaviour, Typeclass}
         || {Typeclass, _Adapter} <- Capabilities],
    GenFunForms =
        lists:append(
          [adapter_forms(Typeclass, Adapter, Attrs#{erlando_type => TypeDeclarations})
           || {Typeclass, Adapter} <- Capabilities]),
    [{attribute, Line, erlando_instance_meta, {1, Metadata}}
     | TypeForms ++ BehaviourForms ++ GenFunForms].

type_declarations(#{type := Type}) ->
    [Type];
type_declarations(#{types := Types}) when is_list(Types), Types =/= [] ->
    Types;
type_declarations(_Spec) ->
    erlang:error(missing_erlando_instance_type).

type_name({Name, _UsedTypes}) when is_atom(Name) ->
    Name;
type_name(Name) when is_atom(Name) ->
    Name;
type_name(Type) ->
    erlang:error({invalid_erlando_instance_type, Type}).

validate_spec(Spec) ->
    case maps:is_key(capabilities, Spec) of
        true -> erlang:error({unsupported_erlando_instance_key, capabilities});
        false -> ok
    end.

capabilities(Spec) ->
    Singular =
        case maps:find(capability, Spec) of
            {ok, Typeclass} when is_atom(Typeclass) ->
                [{Typeclass, maps:get(implementation, Spec, manual)}];
            error ->
                [];
            {ok, Typeclass} ->
                erlang:error({invalid_erlando_instance_capability, Typeclass})
        end,
    AdapterCapabilities =
        normalize_adapter_groups(maps:get(adapters, Spec, [])),
    Manual = normalize_manual_capabilities(maps:get(manual, Spec, [])),
    Singular ++ AdapterCapabilities ++ Manual.

normalize_adapter_groups(Groups) when is_list(Groups) ->
    lists:append([normalize_adapter_group(Group) || Group <- Groups]);
normalize_adapter_groups(Groups) ->
    erlang:error({invalid_erlando_instance_adapters, Groups}).

normalize_adapter_group(
  #{mode := Mode, capabilities := Capabilities} = Group)
  when (Mode =:= source orelse Mode =:= target), is_list(Capabilities) ->
    case valid_adapter_context(Mode, Group)
         andalso lists:all(fun is_atom/1, Capabilities) of
        true ->
            Adapter0 = maps:remove(capabilities, maps:remove(mode, Group)),
            Adapter = Adapter0#{adapter => Mode},
            [{Typeclass, Adapter} || Typeclass <- Capabilities];
        false ->
            erlang:error({invalid_erlando_instance_adapter_group, Group})
    end;
normalize_adapter_group(Group) ->
    erlang:error({invalid_erlando_instance_adapter_group, Group}).

normalize_manual_capabilities(Capabilities) when is_list(Capabilities) ->
    case lists:all(fun is_atom/1, Capabilities) of
        true -> [{Typeclass, manual} || Typeclass <- Capabilities];
        false -> erlang:error({invalid_erlando_instance_manual, Capabilities})
    end;
normalize_manual_capabilities(Capabilities) ->
    erlang:error({invalid_erlando_instance_manual, Capabilities}).

valid_adapter_context(source, Group) ->
    maps:is_key(requires, Group) orelse maps:is_key(args, Group);
valid_adapter_context(target, _Group) ->
    true.

adapter_forms(_Typeclass, manual, _Attrs) ->
    [];
adapter_forms(_Typeclass, generic, _Attrs) ->
    [];
adapter_forms(Typeclass, {dispatch, Dispatch}, Attrs) when is_map(Dispatch) ->
    dispatch_forms(Typeclass, Dispatch, Attrs);
adapter_forms(Typeclass, #{adapter := Mode} = Adapter, Attrs)
  when Mode =:= source; Mode =:= target ->
    Options0 = maps:remove(adapter, maps:remove(requires, Adapter)),
    Options1 =
        case maps:find(requires, Adapter) of
            {ok, InnerType} -> Options0#{inner_type => InnerType};
            error -> Options0
        end,
    Options =
        case Mode of
            source -> Options1#{behaviours => [Typeclass]};
            target -> Options1#{tbehaviours => [Typeclass]}
        end,
    gen_fun_macro:generate_forms(Options, Attrs);
adapter_forms(Typeclass, Adapter, _Attrs) ->
    erlang:error({invalid_erlando_instance_adapter, Typeclass, Adapter}).

instance_metadata(Type, Typeclass, {dispatch, Dispatch}) ->
    #{type => Type,
      typeclass => Typeclass,
      implementation => dispatch,
      dispatch => maps:get(Type, Dispatch)};
instance_metadata(Type, Typeclass, generic) ->
    #{type => Type,
      typeclass => Typeclass,
      implementation => generic,
      adapter => manual};
instance_metadata(Type, Typeclass, Adapter) ->
    #{type => Type,
      typeclass => Typeclass,
      implementation => local,
      adapter => Adapter}.

dispatch_forms(Typeclass, Dispatch, #{pos := Line}) ->
    Callbacks = Typeclass:behaviour_info(callbacks),
    lists:append(
      [dispatch_function(Typeclass, Callback, Arity, Dispatch, Line)
       || {Callback, Arity} <- Callbacks]).

dispatch_function(Typeclass, Callback, Arity, Dispatch, Line) ->
    TypeAdapters = lists:sort(maps:to_list(Dispatch)),
    TupleClauses =
        [dispatch_tuple_clause(Callback, Arity, Type, CallbackAdapters, Line)
         || {Type, CallbackAdapters} <- TypeAdapters],
    AtomClauses =
        [dispatch_atom_clause(Typeclass, Callback, Arity, Type, Line)
         || {Type, _CallbackAdapters} <- TypeAdapters],
    [{attribute, Line, export, [{Callback, Arity}]},
     {function, Line, Callback, Arity, TupleClauses ++ AtomClauses}].

dispatch_tuple_clause(Callback, Arity, Type, CallbackAdapters, Line) ->
    {LocalFunction, Arity} = maps:get(Callback, CallbackAdapters),
    Arguments = argument_variables(Arity - 1, Line),
    Descriptor = {var, Line, 'TypeDescriptor'},
    DescriptorPattern =
        {match, Line, Descriptor,
         {tuple, Line, [{atom, Line, Type}, {var, Line, '_'}]}},
    {clause, Line, Arguments ++ [DescriptorPattern], [],
     [{call, Line, {atom, Line, LocalFunction}, Arguments ++ [Descriptor]}]}.

dispatch_atom_clause(Typeclass, Callback, Arity, Type, Line) ->
    Arguments = argument_variables(Arity - 1, Line),
    DefaultDescriptor =
        {tuple, Line, [{atom, Line, Type}, {atom, Line, Typeclass}]},
    {clause, Line, Arguments ++ [{atom, Line, Type}], [],
     [{call, Line, {atom, Line, Callback}, Arguments ++ [DefaultDescriptor]}]}.

argument_variables(Count, Line) ->
    [{var, Line, list_to_atom("Arg" ++ integer_to_list(N))}
     || N <- lists:seq(1, Count)].

unique_values(Kind, Values) ->
    Unique = lists:usort(Values),
    case length(Values) =:= length(Unique) of
        true -> Unique;
        false -> erlang:error(
                   {duplicate_erlando_instance_declaration, Kind, Values})
    end.

unique_capabilities(Capabilities) ->
    Names = [Name || {Name, _Adapter} <- Capabilities],
    _ = unique_values(capabilities, Names),
    lists:keysort(1, Capabilities).
