%% -------------------------------------------------------------------
%%
%%   cryspi_search: Search routines.
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

-module(cryspi_search).
%-export([solve_goal/1]).
-compile([export_all]).
-export_type([]).

-include("cryspi_syntax.hrl").

-type answer_set() :: [cryspi_unify:unifier()].

-type answer_source() :: [#goal{}].

-record(goal_state,
        {iter_depth=0 :: non_neg_integer()}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif. % TEST.

% copied from OTP 18 maps.erl
-spec maps_filter(Pred,Map1) -> Map2 when
      Pred :: fun((Key, Value) ->
                         boolean()),
      Key  :: term(),
      Value :: term(),
      Map1 :: map(),
            Map2 :: map().
maps_filter(Pred,Map) when is_function(Pred,2), is_map(Map) ->
    maps:from_list([{K,V}||{K,V}<-maps:to_list(Map),
                           Pred(K,V)]);
maps_filter(Pred,Map) ->
    erlang:error(map_error,[Pred,Map]).

-spec merge_answer_sets(AnswersSets::[answer_set()]) -> answer_set().
merge_answer_sets([]) ->
    [];
merge_answer_sets([AS]) ->
    AS;
merge_answer_sets([AS1,AS2|Tail]) ->
    % TODO audit the efficiency of this
    Merged = lists:filtermap(fun ({Ans1, Ans2}) ->
                                     case cryspi_unify:merge(Ans1, Ans2) of
                                         error ->
                                             false;
                                         {ok, Ans} ->
                                             {true, Ans}
                                     end
                             end, [{Ans1, Ans2} || Ans1 <- AS1, Ans2 <- AS2]),
    case Merged of
        [] -> % no merge possible, give up
            [];
        _ -> % else continue reducing the answer set
            merge_answer_sets([Merged|Tail])
    end.

solve_goal(Goal) ->
    solve_goal(Goal, #goal_state{}).

%% @doc Solve a goal.
-spec solve_goal(Goal::#goal{}, GoalState::#goal_state{}) -> answer_set().
solve_goal(#goal{body=Body}, GoalState=#goal_state{iter_depth=IterDepth}) ->
    % search each of the subgoals independently
    % AnswerSets::[answer_set()]
    AnswerSets = lists:map(fun (SG) ->
                                   solve_subgoal(SG, GoalState#goal_state{iter_depth=IterDepth+1})
                           end, Body),
    % merge all the answer sets
    merge_answer_sets(AnswerSets).

% TODO this function should be routed to the appropriate node
-spec solve_subgoal(Literal::cryspi_syntax:literal(), GoalState::#goal_state{}) -> answer_set().
solve_subgoal(Literal, GoalState=#goal_state{iter_depth=Depth}) ->
    % look up all possible answer sources
    {AnswerSources, FactSet} = lookup_predicate(Literal, Depth),
    % filter answers sources whose head unifies with the subgoal
    % AnswerSources::[answer_source()]
    % RuleAnswerSet::answer_set()
    AnswerSet = lists:foldl(fun (G, Acc) ->
                                    [solve_goal(#goal{body=G}, GoalState)|Acc]
                            end, [FactSet], AnswerSources),
    % filter out any irrelevant bits of the unifiers (variables not known at upper layers)
    [maps_filter(fun ({_, VarDepth}, _) -> VarDepth < Depth end, AS) || AS <- lists:flatten(AnswerSet)].

-spec lookup_predicate(Pred::cryspi_syntax:catom(), Depth::non_neg_integer()) -> {answer_source(), answer_set()}.
lookup_predicate({pred, Pred, Args}, Depth) ->
    % TODO using static test data for now
    {OrigRules, OrigFacts} = test_data(Pred),
    Rules = lists:map(fun (R) -> cryspi_syntax:add_depth(R, Depth) end, OrigRules),
    % match heads to predicate arguments via unification
    Bodies = lists:filtermap(fun ({defclause, {pred, _, HeadArgs}, Body}) ->
                                     case cryspi_unify:unify(Args, HeadArgs, #{}) of
                                         {ok, Unifier} ->
                                             {true, lists:map(fun (X) -> cryspi_unify:lookup(X, Unifier) end, Body)};
                                         error ->
                                             false
                                     end
                             end, Rules),
    Facts = lists:filtermap(fun ({pred, _, HeadArgs}) ->
                                    case cryspi_unify:unify(Args, HeadArgs, #{}) of
                                        {ok, Unifier} ->
                                            {true, Unifier};
                                        error ->
                                            false
                                    end
                            end, OrigFacts),
    {Bodies, Facts}.

% ********** TEMPORARY ************* test data
-spec test_data(Pred::string()) -> {[cryspi_syntax:defclause()], % rules
                                    [cryspi_syntax:unit()]}. % facts
test_data(Pred) ->
    Map = #{"p" => % ===============> p(X, Y)
                {[{defclause, {pred, "p", [{var, "X"}, {var, "Y"}]},
                   [{pred, "q", [{var, "X"}, {var, "U"}]},
                    {pred, "v", [{var, "U"}, {var, "W"}]},
                    {pred, "r", [{var, "W"}, {var, "Y"}]}]},
                  {defclause, {pred, "p", [{var, "X"}, {var, "Y"}]},
                   [{pred, "q", [{var, "X"}, {var, "U"}]},
                    {pred, "v", [{var, "U"}, {var, "W"}]},
                    {pred, "r", [{var, "W"}, {var, "Y"}]}]}
                 ], % rules
                 [
                  {pred, "p", [{const, {int, 1}}, {const, {int, 9}}]},
                  {pred, "p", [{const, {int, 2}}, {const, {int, 5}}]},
                  {pred, "p", [{const, {int, 1}}, {const, {int, 7}}]}
                 ]}, % facts
            "v" =>
                {[{defclause, {pred, "v", [{var, "U"}, {var, "V"}]},
                  [{pred, "q", [{var, "U"}, {var, "Z"}]},
                   {pred, "r", [{var, "Z"}, {var, "W"}]}]}
                 ],
                 [
                  {pred, "v", [{const, {int, 2}}, {const, {int, 1}}]},
                  {pred, "v", [{const, {int, 2}}, {const, {int, 3}}]},
                  {pred, "v", [{const, {int, 8}}, {const, {int, 2}}]},
                  {pred, "v", [{const, {int, 7}}, {const, {int, 2}}]}
                 ]},
            "q" =>
                {[],
                 [
                  {pred, "r", [{const, {int, 1}}, {const, {int, 2}}]},
                  {pred, "r", [{const, {int, 1}}, {const, {int, 8}}]},
                  {pred, "r", [{const, {int, 1}}, {const, {int, 4}}]},
                  {pred, "r", [{const, {int, 2}}, {const, {int, 6}}]},
                  {pred, "r", [{const, {int, 2}}, {const, {int, 4}}]},
                  {pred, "r", [{const, {int, 4}}, {const, {int, 9}}]}
                 ]},
            "r" =>
                {[],
                 [
                  {pred, "r", [{const, {int, 1}}, {const, {int, 4}}]},
                  {pred, "r", [{const, {int, 2}}, {const, {int, 4}}]},
                  {pred, "r", [{const, {int, 4}}, {const, {int, 5}}]},
                  {pred, "r", [{const, {int, 5}}, {const, {int, 7}}]}
                 ]},
            "father" =>
                {[],
                 [{pred, "father", [{const, {func, "zeus"}}, {const, {func, "ares"}}]}, % F1. father(zeus, ares).
                  {pred, "father", [{const, {func, "ares"}}, {const, {func, "harmonia"}}]}, % F3. father(ares, harmonia).
                  {pred, "father", [{const, {func, "cadmus"}}, {const, {func, "semele"}}]}, % F5.
                  {pred, "father", [{const, {func, "zeus"}}, {const, {func, "dionysus"}}]} % F7
                 ]},
            "mother" =>
                {[],
                 [{pred, "mother", [{const, {func, "hera"}}, {const, {func, "ares"}}]}, % F2.
                  {pred, "mother", [{const, {func, "aphrodite"}}, {const, {func, "harmonia"}}]}, % F4
                  {pred, "mother", [{const, {func, "harmonia"}}, {const, {func, "semele"}}]}, % F6
                  {pred, "mother", [{const, {func, "semele"}}, {const, {func, "dionysus"}}]} % F8
                 ]},
            "god" =>
                {[],
                 []},
            "female" =>
                {[{defclause, {pred, "female", [{var, "X"}]}, [{pred, "mother", [{var, "X"}, {var, "Y"}]}]} % F14
                 ],
                 []},
            "male" =>
                {[{defclause, {pred, "male", [{var, "X"}]}, [{pred, "father", [{var, "X"}, {var, "Y"}]}]} % F15.
                 ],
                 []},
            "parent" =>
                {[{defclause, {pred, "parent", [{var, "X"}, {var, "Y"}]}, [{pred, "father", [{var, "X"}, {var, "Y"}]}]}, % F17
                  {defclause, {pred, "parent", [{var, "X"}, {var, "Y"}]}, [{pred, "mother", [{var, "X"}, {var, "Y"}]}]} % F16
                 ],
                 []},
            "grandparent" =>
                {[{defclause, {pred, "grandparent", [{var, "X"}, {var, "Y"}]},
                   [{pred, "parent", [{var, "X"}, {var, "Z"}]},
                    {pred, "parent", [{var, "Z"}, {var, "Y"}]}]}
                 ],
                 []}
           },
    case maps:find(Pred, Map) of
        {ok, FR} ->
            FR;
        _ ->
            {[], []} % empty facts and rules
    end.

-ifdef(TEST).
-endif. % TEST.
