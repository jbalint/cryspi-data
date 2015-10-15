%% -------------------------------------------------------------------
%%
%%   cryspi_syntax: Syntactic types used throughout the system.
%%
%%   Copyright 2015 Jess Balint
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% -------------------------------------------------------------------

-module(cryspi_syntax).
-export([is_var/1, is_ground/1, add_depth/2]).

-export_type([var/0, const/0, fterm/0, clist/0, ht/0, cterm/0,
              catom/0, literal/0, defclause/0, goal/0, unit/0, formula/0]).

-type var() :: {var, VarName::string()} | {var, {VarName::string(), Depth::non_neg_integer()}}.
-type const() :: {const, {int, Value::integer()}} |
                 {const, {float, Value::float()}} |
                 {const, {string, Value::string()}} |
                 {const, {func, Value::string()}}.
-type fterm() :: {func, Func::string(), [term()]}.
-type clist() :: {list, [term()]}. % prefixed with "c" to avoid collision with built-in type
-type ht() :: {ht, Head::[term()], Tail::var()}.
-type cterm() :: var() | const() | fterm() | list() | ht().

-type catom() :: {pred, Pred::string(), Args::[term()]}.

-type literal() :: catom(). % TODO: support negation in parser, etc: | {not_, Negated::catom()}.

-type defclause() :: {defclause, Consequent::catom(), Antecedents::[literal()]}.

-type goal() :: {goal, Body::[literal()]}.

-type unit() :: catom().

-type formula() :: defclause() | literal() | cterm().

-spec is_var(Term::cterm()) -> boolean().
is_var({var, _}) ->
    true;
is_var(_) ->
    false.

-spec is_ground(Term::cterm()) -> boolean().
is_ground({var, _}) ->
    false.
% TODO rest of patterns

-spec add_depth(Formula::(formula() | [formula()]), Depth::non_neg_integer()) -> formula().
add_depth({defclause, {pred, Pred, Args}, Body}, Depth) ->
    {defclause, {pred, Pred, lists:map(fun (X) -> add_depth(X, Depth) end, Args)},
     lists:map(fun (X) -> add_depth(X, Depth) end, Body)};
add_depth({goal, Body}, Depth) ->
    {goal, lists:map(fun (X) -> add_depth(X, Depth) end, Body)};
add_depth({pred, Pred, Args}, Depth) ->
    {pred, Pred, lists:map(fun (X) -> add_depth(X, Depth) end, Args)};
add_depth({list, Elements}, Depth) ->
    {list, lists:map(fun (X) -> add_depth(X, Depth) end, Elements)};
add_depth({ht, Head, Tail}, Depth) ->
    {ht, lists:map(fun (X) -> add_depth(X, Depth) end, Head), add_depth(Tail, Depth)};
add_depth({fterm, Func, Args}, Depth) ->
    {fterm, Func, lists:map(fun (X) -> add_depth(X, Depth) end, Args)};
add_depth({var, Name}, Depth) when is_list(Name) ->
    {var, {Name, Depth}};
add_depth(V={var, _}, _) ->
    % force a pattern match failure
    {var, already_has_version} = V,
    V;
% else a constant
add_depth(Formula, _) ->
    Formula.

