-module(cryspi_app).
-behaviour(application).

%-export([start/2, stop/1]).
-compile([export_all]).

start(_Type, _Args) ->
    io:format("Starting 7\n", []),
    {error, "Not yet started"}.

stop(_State) ->
    ok.

