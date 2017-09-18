%% This file is a rewrite of do.erl by ast_traverse.erl
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%

-module(do).

-export([parse_transform/2, format_error/1]).

parse_transform(Forms, _Options) ->
    ast_traverse:map_with_state(fun walk/3, [], Forms).

walk(pre, {call, _Line, {atom, _Line1, do}, [{lc, _Line2, {AtomOrVar, _Line3, _MonadModule} = Monad, _Qs}]} = Node,
     MonadStack)
  when AtomOrVar =:= atom orelse AtomOrVar =:= var orelse AtomOrVar =:= tuple ->
    %% push monad into monad stack when pre parse do block
    {Node, [Monad|MonadStack]};

walk(post, {call, Line, {atom, _Line1, do},
            [{lc, _Line2, {AtomOrVar, _Line3, _MonadModule} = Monad, Qs}]}, [Monad|MonadStack]) 
  when AtomOrVar =:= atom orelse AtomOrVar =:= var orelse AtomOrVar =:= tuple ->
    %% transform do block to monad:bind form and pop monad from monad stack when parse do block
    %% 'do' calls of a particular form:
    %%  do([ MonadMod || Qualifiers ])
    {{call, Line,
      {'fun', Line,
       {clauses,
        [{clause, Line, [], [], do_syntax(Qs, Monad)}]}}, []}, MonadStack};

%%  'return' and 'fail' syntax detection and transformation:
walk(post, {call, Line, {atom, Line1, ReturnOrFail}, As0}, [Monad|_T] = MonadStack)
  when ReturnOrFail =:= return orelse ReturnOrFail =:= fail ->
    %% 'return' calls of a particular form:
    %%  return(Arguments), and
    %% 'fail' calls of a particular form:
    %%  fail(Arguments)
    %% Transformed to:
    %% "monad:return(Monad, Args)" or "monad:fail(Monad, Args)" in monadic context
    {monad_call_expr(Line, Line1, Monad, ReturnOrFail, As0), MonadStack};
walk(_Type, Form, MonadStack) ->
    {Form, MonadStack}.

%%  'do' syntax transformation:
do_syntax([], {_AtomOrVar, MLine, _MonadModule}) ->
    transform_error("A 'do' construct cannot be empty", MLine);
do_syntax([{GenerateOrMatch, Line, _Pattern, _Expr}], _Monad)
  when GenerateOrMatch =:= generate orelse GenerateOrMatch =:= match ->
    transform_error("The last statement in a 'do' construct must be an expression", Line);
do_syntax([{generate, Line,  Pattern, Expr} | Exprs], Monad) ->
    %% "Pattern <- Expr, Tail" where Pattern is a simple variable
    %% is transformed to
    %% "monad:'>>='(Monad, Expr, fun (Pattern) -> Tail')"
    %% without a fail to match clause
    Args = [Expr, {'fun', Line, {clauses, pattern_syntax(Line, Pattern, Exprs, Monad)}}],
    [monad_call_expr(Line, Line, Monad, '>>=', Args)];
do_syntax([Expr], _Monad) ->
    [Expr]; %% Don't do '>>' chaining on the last elem
do_syntax([{match, _Line, _Pattern, _Expr} = Expr | Exprs],
          Monad) ->
    %% Handles 'let binding' in do expression a-la Haskell
    [Expr|do_syntax(Exprs, Monad)];
do_syntax([Expr | Exprs], Monad) ->
    %% "Expr, Tail" is transformed to "monad:'>>='(Monad, Expr, fun (_) -> Tail')"
    %% Line is always the 2nd element of Expr
    Line = element(2, Expr),
    Args = [Expr,
            {'fun', Line,
             {clauses,
              [{clause, Line,
                [{var, Line, '_'}], [], do_syntax(Exprs, Monad)}]}}],
    [monad_call_expr(Line, Line, Monad, '>>=', Args)].

pattern_syntax(Line, {var, _Line, _Var} = Pattern, Exprs, Monad) ->
    [{clause, Line, [Pattern], [], do_syntax(Exprs, Monad)}];
pattern_syntax(Line, Pattern, Exprs, Monad) ->
    %% with a fail clause if the function does not match
    [{clause, Line, [Pattern], [], do_syntax(Exprs, Monad)},
     {clause, Line, [{var, Line, '_'}], [],
      [monad_call_expr(Line, Line, Monad, 'fail', [{atom, Line, 'monad_badmatch'}])]}].

monad_call_expr(Line, Line1, {tuple, _Line2, [{atom, _Line3, _MonadModule} = Module|_T]} = Monad, Function, Args) ->
    %% if Monad is a tuple which is a monad transformer 
    %% call of {Monad, Function, Args} is transformed to {Module, Function, Args ++ [Monad]}
    {call, Line, {remote, Line1, Module, {atom, Line1, Function}}, Args ++ [Monad]};
monad_call_expr(Line, Line1, Monad, Function, Args) ->
    {call, Line, {remote, Line1, {atom, Line1, monad}, {atom, Line1, Function}}, [Monad|Args]}.

%% Use this function to report any parse_transform error. The
%% resulting error message will be displayed as an ordinary
%% compilation error in a standard format.
transform_error(Message, Line) ->
    throw({Message, Line}).

%% This function is called by the Erlang compiler to obtain an error
%% message which will be shown to the user.
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

