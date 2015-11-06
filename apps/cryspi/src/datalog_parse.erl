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
-export([parse/1, l/1, arg_list/1, to_string/1]).

-include("cryspi_syntax.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif. % TEST.

parse(String) ->
    datalog_grammar:parse(String).

% combine head/tail from parser
l(Node) ->
    case Node of
        [H, T] ->
            [H|T];
        _ ->
            Node
    end.

% Filter separators from the arg list
arg_list(Node) ->
    lists:filter(fun is_tuple/1, lists:flatten(Node)).

% List of formulas to a comma-separated string
-spec list_to_string(Formulas::[cryspi_syntax:cterm() | cryspi_syntax:catom()]) -> string().
list_to_string(Formulas) ->
    string:join(lists:map(fun to_string/1, Formulas), ",").

%% @doc Print a formula as a string
-spec to_string(Formula::(cryspi_syntax:cterm() | cryspi_syntax:catom())) -> string().
to_string(Formula) ->
    case Formula of
        {const, {string, Value}} ->
            io_lib:format("\"~s\"", [Value]);
        {const, {func, Value}} ->
            io_lib:format("~s", [Value]);
        {const, {_Type, Value}} ->
            io_lib:format("~p", [Value]);
        {var, Name} ->
            Name;
        {func, Name, Args} ->
            io_lib:format("~s(~s)", [Name, list_to_string(Args)]);
        {list, Args} ->
            io_lib:format("[~s]", [list_to_string(Args)]);
        {pred, Name, Args} ->
            io_lib:format("~s(~s)", [Name, list_to_string(Args)]);
        {defclause, Head, Body} ->
            io_lib:format("~s:-~s.", [to_string(Head), list_to_string(Body)]);
        #goal{body=Body} ->
            io_lib:format("?-~s.", [list_to_string(Body)])
    end.

-ifdef(TEST).
parse_terms_test() ->
    ?assertEqual({const, {int, 1}},
                 datalog_parse:parse("1")),
    ?assertEqual({list, [{const, {int, 1}}, {var, "Xyz"}]},
                 datalog_parse:parse("[1,Xyz]")),
    ?assertEqual({ht, [{const, {int, 1}}], {var, "T"}},
                 datalog_parse:parse("[1|T]")).

to_string_test() ->
    ?assertEqual("[1,2,a,B]",
                 lists:flatten(to_string(datalog_parse:parse("[1,2,a,B]")))),
    ?assertEqual("a(b):-c(d),e(f).",
                 lists:flatten(to_string(datalog_parse:parse("a(b):-c(d),e(f).")))),
    ?assertEqual("?-a(b),c(d).",
                 lists:flatten(to_string(datalog_parse:parse("?-a(b),c(d).")))),
    ?assertEqual("f(g(X)):-a(X),b(X,1).",
                 lists:flatten(to_string(datalog_parse:parse("f(g(X)):-a(X),b(X,1).")))).

basic_defclause_test() ->
    ?assertEqual({defclause,
                  {pred, "female", [{var, "X"}]},
                  [{pred, "mother", [{var, "X"}, {var, "Y"}]}]},
                 datalog_parse:parse("female(X):-mother(X,Y).")),
    ?assertEqual({defclause,
                  {pred, "a", [{const, {int, 1}}]},
                  [{pred, "b", [{const, {int, 2}}]},
                   {pred, "c", [{const, {int, 3}}]}]},
                 datalog_parse:parse("a(1):-b(2),c(3).")).

basic_goal_test() ->
    ?assertEqual(#goal{body=[{pred, "a", [{var, "B"}]},
                             {pred, "b", [{var, "C"}, {const, {int, 1}}]}]},
                 datalog_parse:parse("?-a(B),b(C,1).")).

-endif. % TEST.
