-module(datalog_parse).
-compile(export_all).

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
    {f, Sym, Args2}.

arg_list(Node) ->
    lists:filter(fun is_tuple/1, lists:flatten(Node)).

unit_clause(Node) ->
    case Node of
        [<<"(">>, Disj, <<")">>] ->
            Disj;
        _ ->
            Node
    end.

%% @doc Print a formula as a string
%%%%%%%%%%%%%%%%%%-spec to_string(Formula::formula()) -> iolist().
to_string(Formula) ->
    case Formula of
        {const, Name} ->
            Name;
        {f, Name, Args} ->
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
