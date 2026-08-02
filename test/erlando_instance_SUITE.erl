-module(erlando_instance_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [test_state_t_metadata_is_retained,
     test_identity_metadata_and_target_adapter,
     test_monad_cont_dispatch_metadata,
     test_all_metadata_mappings_are_registered,
     test_remote_adapter_group,
     test_top_level_capabilities_is_rejected,
     test_duplicate_capability_is_rejected,
     test_source_adapter_requires_context].

test_state_t_metadata_is_retained(_Config) ->
    Beam = filename:join([code:lib_dir(erlando), "ebin", "state_t.beam"]),
    {ok, {state_t, [{attributes, Attributes}]}} =
        beam_lib:chunks(Beam, [attributes]),
    Metadata = proplists:get_value(erlando_instance_meta, Attributes),
    ?assertMatch([{1, #{module := state_t}}], Metadata),
    GenFunMetadata = proplists:get_value(gen_fun_meta, Attributes),
    ?assert(lists:all(fun({1, Options}) -> is_map(Options) end,
                      GenFunMetadata)),
    ?assert(lists:any(
              fun({1, Options}) ->
                      maps:get(behaviours, Options, []) =:= [functor]
              end, GenFunMetadata)),
    [{1, Normalized}] = Metadata,
    Instances = maps:get(instances, Normalized),
    Mappings = lists:sort(
                 [{maps:get(type, Instance), maps:get(typeclass, Instance)}
                  || Instance <- Instances]),
    ?assertEqual(
       lists:sort(
         [{state_t, functor},
          {state_t, applicative},
          {state_t, monad},
          {state_t, monad_trans},
          {state_t, monad_state},
          {state_t, alternative},
          {state_t, monad_plus},
          {state_t, monad_runner}]),
       Mappings).

test_identity_metadata_and_target_adapter(_Config) ->
    Metadata = instance_metadata(identity),
    Instances = maps:get(instances, Metadata),
    ?assertEqual(
       lists:sort([functor, applicative, monad, monad_fail, monad_runner]),
       lists:sort([maps:get(typeclass, Instance) || Instance <- Instances])),
    ?assertEqual({identity, mapped}, functor:fmap(fun(_) -> mapped end,
                                                  {identity, value})).

instance_metadata(Module) ->
    Ebin = filename:join(code:lib_dir(erlando), "ebin"),
    Beam = filename:join(Ebin, atom_to_list(Module) ++ ".beam"),
    {ok, {Module, [{attributes, Attributes}]}} =
        beam_lib:chunks(Beam, [attributes]),
    [{1, Metadata}] = proplists:get_value(erlando_instance_meta, Attributes),
    Metadata.

test_monad_cont_dispatch_metadata(_Config) ->
    Metadata = instance_metadata(monad_cont_instance),
    Instances = maps:get(instances, Metadata),
    ?assertEqual(
       lists:sort([reader_t, writer_t, state_t, maybe_t,
                   error_t, except_t, list_t]),
       lists:sort([maps:get(type, Instance) || Instance <- Instances])),
    Adapters =
        #{reader_t => reader_t_call_cc,
          writer_t => writer_t_call_cc,
          state_t => state_t_call_cc,
          maybe_t => maybe_t_call_cc,
          error_t => error_t_call_cc,
          except_t => except_t_call_cc,
          list_t => list_t_call_cc},
    lists:foreach(
      fun(Instance) ->
              ?assertEqual(dispatch, maps:get(implementation, Instance)),
              ?assertEqual({maps:get(maps:get(type, Instance), Adapters), 2},
                           maps:get(callCC, maps:get(dispatch, Instance)))
      end, Instances),
    ?assert(erlang:function_exported(monad_cont_instance, callCC, 2)).

test_all_metadata_mappings_are_registered(_Config) ->
    _ = application:load(erlando),
    {ok, Modules} = application:get_key(erlando, modules),
    Mappings =
        lists:append(
          [module_mappings(Module)
           || Module <- Modules]),
    ?assert(length(Mappings) > 50),
    ?assertEqual(length(Mappings), length(lists:usort(Mappings))),
    lists:foreach(
      fun({Type, Typeclass, Module}) ->
              ?assertEqual(Module, typeclass:module(Type, Typeclass))
      end, Mappings).

module_mappings(Module) ->
    Beam = filename:join([code:lib_dir(erlando), "ebin",
                          atom_to_list(Module) ++ ".beam"]),
    case beam_lib:chunks(Beam, [attributes]) of
        {ok, {Module, [{attributes, Attributes}]}} ->
            MetadataValues =
                lists:flatten(
                  proplists:get_all_values(erlando_instance_meta, Attributes)),
            [{maps:get(type, Instance), maps:get(typeclass, Instance), Module}
             || {1, Metadata} <- MetadataValues,
                Instance <- maps:get(instances, Metadata)];
        _ ->
            []
    end.

test_remote_adapter_group(_Config) ->
    ?assertEqual(environment,
                 reader_m:run(monad_reader:ask(reader_m), environment)).

test_top_level_capabilities_is_rejected(_Config) ->
    Spec =
        #{type => fixture_type,
          capabilities => [monad_runner]},
    ?assertError(
       {unsupported_erlando_instance_key, capabilities},
       erlando_instance_macro:erlando_instance(
         Spec, #{module => fixture_instance, pos => 1})).

test_duplicate_capability_is_rejected(_Config) ->
    Spec =
        #{type => duplicate_fixture,
          adapters =>
              [#{mode => target,
                 patterns => [duplicate_fixture],
                 capabilities => [monad_runner]}],
          manual => [monad_runner]},
    ?assertError(
       {duplicate_erlando_instance_declaration,
        capabilities, [monad_runner, monad_runner]},
       erlando_instance_macro:erlando_instance(
         Spec, #{module => duplicate_fixture, pos => 1})).

test_source_adapter_requires_context(_Config) ->
    Group = #{mode => source, capabilities => [functor]},
    Spec = #{type => invalid_source_fixture, adapters => [Group]},
    ?assertError(
       {invalid_erlando_instance_adapter_group, Group},
       erlando_instance_macro:erlando_instance(
         Spec, #{module => invalid_source_fixture, pos => 1})).
