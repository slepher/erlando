%% ``Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(ast_traverse).

%% An identity transformer of Erlang abstract syntax.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for transforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([attributes/2, map_reduce/3, traverse/2, traverse/3]).

attributes(Attribute, Forms) ->
    lists:foldl(
      fun({attribute, _Line, Attr, Values}, Acc) when Attr == Attribute ->
              [Values|Acc];
         (_Other, Acc) ->
              Acc
      end, [], Forms).

-spec map_reduce(fun((_Type, Node, State) -> error_m:error_m(any(), {Node, State})), State, Form) ->
                        error_m:error_m(any(), {Form, State}).
map_reduce(F, Init, Forms) ->
    ST = state_t:new(error_m),
    STNode = ast_traverse:traverse(
               ST, fun(Type, Node) -> state_t:state_t(fun(State) -> F(Type, Node, State) end) end, Forms),
    state_t:run_state(STNode, Init, ST).

-spec traverse(fun((Node, _Type) -> Node), Form) -> Form.
traverse(F, Forms) ->
    traverse(identity_m, F, Forms).

-spec traverse(M, fun((_Type, Node) -> monad:monadic(M, Node)), Form) -> monad:monadic(M, Form) when M :: monad:monad().
traverse(Monad, F, Forms) ->
    do_traverse(Monad, F, Forms, ast_lens:forms(Forms)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_traverse(Monad, F, Node, ChildrenLens) ->
    %% do form
    %% do([Monad ||
    %%           F({pre, Node}),
    %%           NNode <- fold_children(Monad, F, Node, ChildrenLens),
    %%           F(NNode)
    %%    ]).
    monad:bind(
      Monad,F(pre, Node),
      fun(NNode) ->
              monad:bind(
                Monad,
                fold_children(Monad, F, NNode, ChildrenLens),
                fun(NNNode) ->
                        F(post, NNNode)
                end)
      end).

fold_children(Monad, F, Node, ChildrenLens) ->
    lists:foldl( 
      fun({CChildrenLensVisitor, ChildLens}, MNode) ->
              monad:bind(Monad, MNode,
                          fun(NodeAcc) ->
                                  (ast_lens:modify(Monad, ChildLens, 
                                                   fun(Child) ->
                                                           CChildrenLens = CChildrenLensVisitor(Child),
                                                           do_traverse(Monad, F, Child, CChildrenLens)
                                                   end))(NodeAcc)
                          end)
      end, monad:return(Monad, Node), ChildrenLens).
