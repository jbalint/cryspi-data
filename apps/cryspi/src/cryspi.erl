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
    io:format("Add rule: ~s\n", [datalog_parse:to_string({rule, {func, Name, Args}, Body})]),
    State2;
process_parsed({assert, {func, Name, Args}}, State) ->
    State2 = assert(Name, {fact, Args}, State),
    io:format("Asserted fact: ~s\n", [datalog_parse:to_string({func, Name, Args})]),
    State2;
process_parsed({goal, {func, Name, Args}}, State) ->
    case get(Name) of
        undefined ->
            undefined;
        Stuff ->
            query(Stuff, Args, State)
    end,
    State.

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
    process("q(1,2)."),
    process("q(1,8)."),
    process("q(1,4)."),
    process("q(2,6)."),
    process("q(2,4)."),
    process("q(4,9)."),
    process("r(2,4)."),
    process("r(4,5)."),
    process("r(5,7)."),
    get().

% Query for predicate facts that unify with the given arguments. The
% argument is a list of facts as they're stored. {fact, Name,
% Args}. The return value is a set of args tuples with the matches
% replacing the variables. (Is this forcing us into too many
% unification operations?)
query(Facts, Args, State) ->
    % attempt unification with all facts
    Unifiers1 = lists:map(fun ({fact, FactArgs}) -> unify:unify(Args, FactArgs, #{}) end, Facts),
    % filter out non-matches
    Unifiers = lists:map(fun ({ok, U}) -> U end, lists:filter(fun (Res) -> case Res of {ok, _} -> true ; _ -> false end end, Unifiers1)),
    % replace the variables with the args according to the result
    ReplacedArgs = lists:map(fun (Unifier) -> lists:map(fun (Arg) -> unify:lookup(Arg, Unifier) end, Args) end, Unifiers),
    ReplacedArgs.

assert(Name, Thing, State=#state{pred_table=Table}) ->
    Pred = case ets:lookup(Table, Name) of
               [] ->
                   #pred{facts=[], rules=[]};
               [{Name, P}] ->
                   P
           end,
    Pred2 = assert(Name, Thing, Pred),
    ets:insert(Table, {Name, Pred2}),
    State;
assert(Name, Fact={fact, _}, Pred=#pred{facts=Facts}) ->
    Pred#pred{facts=[Fact|Facts]};
assert(Name, Rule={rule, _, _}, Pred=#pred{rules=Rules}) ->
    Pred#pred{rules=[Rule|Rules]}.

