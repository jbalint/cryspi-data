%% -------------------------------------------------------------------
%%
%%   unify: Unification of terms.
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

-module(unify).
-export([merge/2, lookup/2, unify/3]).

-include("syntax.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif. % TEST.

% @doc "Merge" two unifiers. Essentially a merge of the mappings
% verifying that any shared keys map to the same value. If any shared
% keys map to two different values, then an "error" is returned.
-spec merge(U1::unifier(), U2::unifier()) -> {ok, unifier()} | error.

merge(U1, U2) ->
    case merge_impl(U1, U2, maps:keys(U1)) of
        error ->
            error;
        UMerge ->
            {ok, UMerge}
    end.

merge_impl(_, U2, []) ->
    U2;
merge_impl(U1, U2, [K|Tail]) ->
    case maps:find(K, U1) of
        {ok, U1Val} ->
            case maps:find(K, U2) of
                {ok, U2Val} -> % in both, verify equality
                    if U1Val == U2Val ->
                            merge_impl(U1, U2, Tail);
                       true ->
                            error
                    end;
                _ -> % Not in U2, add it
                    merge_impl(U1, maps:put(K, U1Val, U2), Tail)
            end;
        _ -> % not in U2, already in U2, move on
            merge_impl(U1, U2, Tail)
    end.

% @doc Replace any variables in the given term according to the
% unifier
-spec lookup(Term::term_node(), Unifier::unifier()) -> term_node().

lookup({var, V}, Unifier) ->
    case maps:find(V, Unifier) of
        error ->
            {var, V};
        {ok, Term} ->
            lookup(Term, Unifier)
    end;
lookup({func, Name, Args}, Unifier) ->
    {func, Name, lists:map(fun (Arg) -> lookup(Arg, Unifier) end, Args)};
lookup(Term, _) ->
    Term.

% @doc Unify two sequences of terms in "terms of" the first sequence.
-spec unify(Terms1::[term_node()], Terms2::[term_node()], Unifier::unifier()) -> {ok, unifier()} | error.

unify([], [], Unifier) -> {ok, Unifier};

unify(Terms1, Terms2, _) when length(Terms1) /= length(Terms2) -> error;

unify([{var, V1}|Tail1], [{var, V2}|Tail2], Unifier) when V1 == V2 ->
    unify(Tail1, Tail2, Unifier);

unify([{var, V1}|Tail1], [{var, V2}|Tail2], Unifier) ->
    case {maps:find(V1, Unifier), maps:find(V2, Unifier)} of
        % both variables are bound
        {{ok, Term1}, {ok, Term2}} ->
            unify([Term1|Tail1], [Term2|Tail2], Unifier);
        % neither are bound, bind V2 => V1
        {error, error} ->
            unify(Tail1, Tail2, maps:put(V2, {var, V1}, Unifier));
        % one of the two is already bound. Bind the unbound to the value of the bound
        {{ok, Term1}, error} ->
            unify(Tail1, Tail2, maps:put(V2, Term1, Unifier));
        {error, {ok, Term2}} ->
            unify(Tail1, Tail2, maps:put(V1, Term2, Unifier))
    end;

% Term is a ground term. vars are handled above
unify([{var, V}|Tail1], [GroundTerm|Tail2], Unifier) ->
    case maps:find(V, Unifier) of
        {ok, VTerm} ->
            unify([VTerm|Tail1], [GroundTerm|Tail2], Unifier);
        error ->
            unify(Tail1, Tail2, maps:put(V, GroundTerm, Unifier))
    end;
% Inverse
unify([GroundTerm|Tail1], [{var, V}|Tail2], Unifier) ->
    case maps:find(V, Unifier) of
        {ok, VTerm} ->
            unify([GroundTerm|Tail1], [VTerm|Tail2], Unifier);
        error ->
            unify(Tail1, Tail2, maps:put(V, GroundTerm, Unifier))
    end;

unify([Ground1|Tail1], [Ground2|Tail2], Unifier) when Ground1 == Ground2 ->
    unify(Tail1, Tail2, Unifier);

% TODO lists

unify([{func, Name1, Args1}|Tail1], [{func, Name2, Args2}|Tail2], Unifier) ->
    case Name1 of
        Name2 ->
            % let unify/3 make sure args lists are the same length
            unify(Args1 ++ Tail1, Args2 ++ Tail2, Unifier);
        _SomethingElse ->
            error
    end;

unify(_, _, _) -> error. % same return value as maps:find/2 ... ok?

-ifdef(TEST).
simple_test() ->
    [?assertEqual({ok, #{"X" => {const, "asd"}}},
                  unify([{var, "X"}], [{const, "asd"}], #{})),
     ?assertEqual({ok, #{"X" => {const, "asd"}}},
                  unify([{const, "asd"}], [{var, "X"}], #{})),
     ?assertEqual({ok, #{"X" => {const, "asd"}}},
                  unify([{const, "asd"}, {const, "asd"}],
                        [{var, "X"},     {var, "X"}],
                        #{})),
     ?assertEqual({ok, #{"X" => {const, "asd"}, "Y" => {const, "asd"}}},
                  unify([{const, "asd"}, {const, "asd"}, {var, "X"}],
                        [{var, "Y"},     {var, "X"},     {var, "Y"}],
                        #{})),
     ?assertEqual({ok, #{"X" => {const, "asd"}, "Y" => {var, "X"}}},
                  ?LET(Unifier, unify([{var, "X"}, {const, "asd"}],
                                      [{var, "Y"},     {var, "X"}],
                                      #{}),
                       Unifier)),
     ?assertEqual({ok, #{"X" => {const, "asd"}, "Y" => {const, "asd"}}},
                  unify([{const, "asd"}, {var, "Y"}],
                        [{var, "Y"},     {var, "X"}],
                        #{})),
     ?assertEqual(error,
                  unify([{func, "asd", []}], [{func, "asdf", []}], #{})),
     ?assertEqual({ok,#{"X" => {const, "asd"}}},
                  unify([{func, "asd", [{var, "X"}]}],
                        [{func, "asd", [{const, "asd"}]}], #{})),
     % diff # of args
     ?assertEqual(error,
                  unify([{func, "a", [{const, "a"}, {const, "b"}]}],
                        [{func, "a", [{const, "a"}]}], #{})),
     % Don't bind a variable to itself
     ?assertEqual({ok, #{}},
                   unify([{var, "X"}], [{var, "X"}], #{}))
    ].

defined_cases_test() ->
    [% Y = f(W), Z = f(a), Y = Z.
     ?assertEqual({ok, #{"W" => {const, "a"},
                         "Y" => {func, "f", [{var, "W"}]},
                         "Z" => {func, "f", [{const, "a"}]}}},
                  unify([{func, "f", [{var, "W"}]}, {func, "f", [{const, "a"}]}, {var, "Z"}],
                        [{var, "Y"},             {var, "Z"},               {var, "Y"}],
                        #{})),
     % Z = f(X), Y = f(W), W = f(a), Y = Z
     ?assertEqual({ok, #{"W" => {func, "f", [{const, "a"}]},
                         "X" => {func, "f", [{const, "a"}]},
                         "Y" => {func, "f", [{var, "W"}]},
                         "Z" => {func, "f", [{var, "X"}]}}},
                  unify([{func, "f", [{var, "X"}]}, {func, "f", [{var, "W"}]}, {func, "f", [{const, "a"}]}, {var, "Z"}],
                        [{var, "Z"},                {var, "Y"},                {var, "W"},                  {var, "Y"}],
                        #{})),
     % same formula as previous test with variable lookup
     ?assertEqual({{func, "some_func", [{func, "f", [{func, "f", [{const, "a"}]}]}]},
                   {func, "f", [{func, "f", [{const, "a"}]}]}},
                  ?LET({ok, Unifier},
                       unify([{func, "f", [{var, "X"}]}, {func, "f", [{var, "W"}]}, {func, "f", [{const, "a"}]}, {var, "Z"}],
                             [{var, "Z"},                {var, "Y"},                {var, "W"},                  {var, "Y"}],
                             #{}),
                       {lookup({func, "some_func", [{var, "Z"}]}, Unifier),
                        lookup({var, "Y"}, Unifier)}))
    ].

long_test() ->
    % T1 = p(Z, h(Z, W), f(W)), T2 = p(f(X), h(Y, f(a)), Y), T1 = T2.
    % this literal of internal representation is admittedly a bit much, but a good test
    T1 = {func, "p", [{var, "Z"},                {func, "h", [{var, "Y"}, {func, "f", [{const, "a"}]}]}, {var, "Y"}]},
    T2 = {func, "p", [{func, "f", [{var, "X"}]}, {func, "h", [{var, "Z"}, {var, "W"}]},                  {func, "f", [{var, "W"}]}]},
    ?assertEqual({ok,#{"T1" => T1,
                       "T2" => T2,
                       "W" => {func, "f", [{const, "a"}]},
                       "X" => {func, "f", [{const, "a"}]},
                       "Y" => {func, "f", [{var, "X"}]},
                       "Z" => {func, "f", [{var, "X"}]}}},
                 unify([T1,          T2,          {var, "T2"}],
                       [{var, "T1"}, {var, "T2"}, {var, "T1"}],
                       #{})).

merge_test() ->
    % subsumes
    Test1U1 = #{"X" => "X"},
    Test1U2 = #{"X" => "X", "Y" => "Y"},
    ?assertEqual({ok, Test1U2}, merge(Test1U1, Test1U2)),
    % overlapping
    Test2U1 = #{"X" => "X", "Y" => "Y"},
    Test2U2 = #{"Y" => "Y", "Z" => "Z"},
    ?assertEqual({ok, #{"X" => "X", "Y" => "Y", "Z" => "Z"}},
                 merge(Test2U1, Test2U2)),
    % not-matching
    Test3U1 = #{"X" => "X1"},
    Test3U2 = #{"X" => "X2"},
    ?assertEqual(error, merge(Test3U1, Test3U2)).

-endif. % TEST.
