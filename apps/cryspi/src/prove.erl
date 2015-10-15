%% -------------------------------------------------------------------
%%
%%   prove: Proof routines.
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

-module(prove).
-export([query_facts/2, query_rules/2, merge_rule_unifiers/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif. % TEST.

% @doc Query facts which unify with the arguments. Return a sequence
% of unifiers. One unifier is return for every matching fact.
%-spec query_facts(Args::[term_node()], Facts::[[term_node()]]) -> [unifier()].

query_facts(Args, Facts) ->
    lists:filtermap(fun (F) ->
                            case cryspi_unify:unify(Args, F, #{}) of
                                error ->
                                    false;
                                {ok, Unifier} ->
                                    {true, Unifier}
                            end
                    end, Facts).

% @doc Query rules which unify with the argument list and return a
% list of sub-goals.
%-spec query_rules(Args::[term_node()], Rules::[rule_node()]) -> [[func_node()]].

query_rules(Args, Rules) ->
    % which rules have heads that unify with `Args'?
    RulesAndUnifiers = lists:filtermap(fun (R={rule, RuleArgs, _}) ->
                                               case cryspi_unify:unify(Args, RuleArgs, #{}) of
                                                   error ->
                                                       false;
                                                   {ok, Unifier} ->
                                                       {true, {R, Unifier}}
                                               end
                                       end, Rules),
    % formulate rules in terms of `Args'
    lists:map(fun ({{rule, _, Atoms}, Unifier}) -> % once for each {Rule, Unifier} pair
                      lists:map(fun ({func, Name, FArgs}) ->
                                        {func, Name, [cryspi_unify:lookup(Arg, Unifier) || Arg <- FArgs]}
                                end, Atoms)
              end, RulesAndUnifiers).

% @doc Merge rule unifiers from individual predicate unifiers lists. A
% single rule (generally) has more than one predicate. Each predicate
% can have many unifiers. The lists of unifiers are combined here to
% find answers for the rule.
%-spec merge_rule_unifiers(Unifiers::[[unifier()]]) -> [unifier()].

merge_rule_unifiers([Unifiers1,Unifiers2|Tail]) ->
    Merged = [cryspi_unify:merge(U1, U2) || U1 <- Unifiers1, U2 <- Unifiers2],
    % filter out conflicting pairs
    Merged2 = lists:filtermap(fun (error) -> false;
                                  ({ok, X}) -> {true, X}
                              end, Merged),
    merge_rule_unifiers([Merged2|Tail]);
merge_rule_unifiers([Unifiers]) ->
    Unifiers;
merge_rule_unifiers([]) ->
    [].

-ifdef(TEST____________DISABLED).

query_simple_facts_test() ->
    Args = [{const, "a"}, {var, "X"}],
    Facts = [[{const, "a"}, {const, "b"}],
             [{const, "a"}, {const, "c"}],
             [{const, "B"}, {var, "M"}],
             [{const, "a"}, {var, "M"}]],
    % Note, the order here is implementation dependent but not
    % required by the contract
    ExpectedResult = [#{"X" => {const, "b"}},
                      #{"X" => {const, "c"}},
                      #{"M" => {var, "X"}}],
    ?assertEqual(ExpectedResult, prove:query_facts(Args, Facts)).

query_simple_rules_test() ->
    Args = [{literal, 1}, {var, "M"}],
    Rules = [{rule,
              [{var, "X"}, {var, "Y"}],
              [{func, "q", [{var, "X"}, {var, "U"}]},
               {func, "v", [{var, "U"}, {var, "W"}]},
               {func, "r", [{var, "W"}, {var, "Y"}]}]}],
    ExpectedResult = [[{func, "q", [{literal, 1}, {var, "U"}]},
                       {func, "v", [{var, "U"}, {var, "W"}]},
                       {func, "r", [{var, "W"}, {var, "M"}]}]],
    ?assertEqual(ExpectedResult,
                 prove:query_rules(Args, Rules)).

query_simple_rules2_test() ->
    Args = [{var, "X"}, {var, "Y"}],
    Rules = [{rule,
              [{var, "U"}, {var, "W"}],
              [{func, "q", [{var, "U"}, {var, "Z"}]},
               {func, "r", [{var, "Z"}, {var, "W"}]}]}],
    ExpectedResult = [[{func, "q", [{var, "X"}, {var, "Z"}]},
                       {func, "r", [{var, "Z"}, {var, "Y"}]}]],
    ?assertEqual(ExpectedResult,
                 prove:query_rules(Args, Rules)).

-endif. % TEST.
