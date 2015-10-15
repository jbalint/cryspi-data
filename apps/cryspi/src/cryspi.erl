%% -------------------------------------------------------------------
%%
%%   cryspi: A deductive database.
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

-module(cryspi).
-compile([export_all]).

-record(state, {pred_table :: ets:tid()}).

-record(pred, {facts :: [any()],
               rules :: [any()]}).

% Can Dialyzer figure stuff out like this?
%-type x() :: #{string() => string()}.
%-spec my_func(x()) -> x().
%my_func(_) -> #{x => x}.

process(Command) ->
    Tree = datalog_parse:parse(Command),
    State = case get(state) of
                undefined ->
                    #state{pred_table=ets:new(cryspi_preds, [named_table])};
                S ->
                    S
            end,
    State2 = process_parsed(Tree, State),
    put(state, State2),
    ok.

process_parsed({rule, {func, Name, Args}, Body}, State) ->
    State2 = assert(Name, {rule, Args, Body}, State),
    %io:format("Add rule: ~s\n", [datalog_parse:to_string({rule, {func, Name, Args}, Body})]),
    State2;
process_parsed({assert, {func, Name, Args}}, State) ->
    State2 = assert(Name, {fact, Args}, State),
    io:format("Asserted fact: ~s\n", [datalog_parse:to_string({func, Name, Args})]),
    State2;
process_parsed({goal, Func}, State=#state{pred_table=Table}) ->
    query(Func, Table),
    State.

%-spec query(Func::func_node(), ets:tid()) -> [unifier()].

query({func, Name, Args}, Table) ->
    % "locate" the predicate
    case ets:lookup(Table, Name) of
        [] ->
            %io:format("~p: ~p\n", [datalog_parse:to_string(Func), #{}]);
            [];
        [{_, Pred}] ->
            query_pred(Args, Pred, Table)
            %% lists:map(fun (U) ->
            %%                   io:format("~p: ~p\n", [datalog_parse:to_string(Func), U])
            %%           end, Unifiers)
    end.

%-spec query_pred(Args::[term_node()], Pred::#pred{}, Table::ets:tid()) -> [unifier()].

query_pred(Args, #pred{facts=Facts, rules=Rules}, Table) ->
    FactUnifiers = prove:query_facts(Args, Facts),
    % TODO this has to iterate for EACH RULE
    io:format("Args: ~p\n", [Args]),
    io:format("Rules: ~p\n", [Rules]),
    SubGoals = case prove:query_rules(Args, Rules) of
                   [] ->
                       [];
                   [S] ->
                       S
               end,
    %io:format("SubGoals: ~p\n", [SubGoals]),
    % Need a way to final where/how to solve these subgoals
    SubGoalUnifiers = lists:map(fun (Func) ->
                                        query(Func, Table)
                                end, SubGoals),
    %io:format("SubGoalUnifiers: ~p\n", [SubGoalUnifiers]),
    Flattened = prove:merge_rule_unifiers(SubGoalUnifiers),
    FactUnifiers ++ Flattened.

assert(Name, Thing, State=#state{pred_table=Table}) ->
    Pred = case ets:lookup(Table, Name) of
               [] ->
                   #pred{facts=[], rules=[]};
               [{_Name, P}] ->
                   P
           end,
    Pred2 = assert(Name, Thing, Pred),
    ets:insert(Table, {Name, Pred2}),
    State;
assert(_, {fact, Args}, Pred=#pred{facts=Facts}) ->
    Pred#pred{facts=[Args|Facts]};
assert(_, Rule={rule, _, _}, Pred=#pred{rules=Rules}) ->
    Pred#pred{rules=[Rule|Rules]}.

x() ->
    erase(),
    % delete ETS table if it exists
    case ets:info(cryspi_preds) of
        undefined ->
            ok;
        _ ->
            ets:delete(cryspi_preds)
    end,
    process("p(X,Y):-q(X,U),v(U,W),r(W,Y)."),
    process("p(1,9)."),
    process("p(2,5)."),
    process("p(1,7)."),
    process("v(U,W):-q(U,Z),r(Z,W)."),
    process("v(2,1)."),
    process("v(2,3)."),
    process("v(8,2)."),
    process("v(7,2)."),
    process("q(1,2)."),
    process("q(1,8)."),
    process("q(1,4)."),
    process("q(2,6)."),
    process("q(2,4)."),
    process("q(4,9)."),
    process("r(1,4)."),
    process("r(2,4)."),
    process("r(4,5)."),
    process("r(5,7)."),
    get().
%cryspi:query({func, "p", [{literal,1},{var,"M"}]},cryspi_preds).

y() ->
    erase(),
    % delete ETS table if it exists
    case ets:info(cryspi_preds) of
        undefined ->
            ok;
        _ ->
            ets:delete(cryspi_preds)
    end,
    process("likes(bob,logic)."),
    process("likes(bob,SOMEONE):-likes(SOMEONE,logic)."),
    process("?-likes(bob,WHO)."),
    io:format("X---->\n", []),
    query({func, "likes", [{const, "bob"}, {var, "WHO"}]}, cryspi_preds).
