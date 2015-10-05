%% -------------------------------------------------------------------
%%
%%   datalog_parse: A parser for datalog assertions, queries, rules.
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

%% @doc Datalog parser.

-module(datalog_parse).
-compile(export_all).

-include("syntax.hrl").

parse(String) ->
    datalog_grammar:parse(String).

l(Node) ->
    case Node of
        [H, T] ->
            [H|T];
        _ ->
            Node
    end.

rule(Node) ->
    [Head | [_Notation, Body, _Period]] = Node, % head/body are datalog jargon
    {rule, Head, Body}.

goal([_Q, F, _Dot]) ->
    {goal, F}.

assert([F, _Dot]) ->
    {assert, F}.

disj(Node) ->
    Formulas = lists:filter(fun is_tuple/1, lists:flatten(Node)),
    case Formulas of
        [F] ->
            F;
        _ ->
            {disj, Formulas}
    end.

conj(Node) ->
    Formulas = lists:filter(fun is_tuple/1, lists:flatten(Node)),
    case Formulas of
        [F] ->
            F;
        _ ->
            {conj, Formulas}
    end.

func(Node) ->
    [Sym | [_, Args, _]] = Node,
    Args2 = Args,%lists:filter(fun is_tuple/1, Args),
    {func, Sym, Args2}.

arg_list(Node) ->
    lists:filter(fun is_tuple/1, lists:flatten(Node)).

literal(Node) ->
    {literal, Node}.

unit_clause(Node) ->
    case Node of
        [<<"(">>, Disj, <<")">>] ->
            Disj;
        _ ->
            Node
    end.

var(Node) ->
    {var, Node}.

%% @doc Print a formula as a string
to_string(Formula) ->
    case Formula of
        {const, Name} ->
            Name;
        {var, Name} ->
            Name;
        {literal, Value} ->
            io_lib:format("~p", [Value]);
        {func, Name, Args} ->
            io_lib:format("~s(~s)", [Name, string:join(lists:map(fun to_string/1, Args), ",")]);
        {conj, Formulas} ->
            io_lib:format("(~s)", [string:join(lists:map(fun to_string/1, Formulas), ",")]);
        {disj, Formulas} ->
            io_lib:format("(~s)", [string:join(lists:map(fun to_string/1, Formulas), ";")]);
        {rule, Head, Body} ->
            io_lib:format("~s:-~s.", [to_string(Head), to_string(Body)]);
        _ ->
            io_lib:format("<UNK: ~p>", [Formula])
    end.
