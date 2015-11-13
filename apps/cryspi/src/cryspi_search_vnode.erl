%% -------------------------------------------------------------------
%%
%%   cryspi_search_vnode: Vnode backend for search functionality.
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

-module(cryspi_search_vnode).
-behaviour(riak_core_vnode).
-include_lib("riak_core/include/riak_core_vnode.hrl").
% TODO don't export all
-compile([export_all]).

init(_Partitions) ->
    {ok, {}}.

handle_command(_Request, _Sender, ModState) ->
    Result = nothing,
    {reply, Result, ModState}.

handle_coverage(_Request, _Keyspaces, _Sender, ModState) ->
    % Unsupported (can implement higher order queries)
    {noreply, ModState}.

handle_exit(_Pid, _Reason, ModState) ->
    {noreply, ModState}.

handoff_starting(_Dest, ModState) ->
    {false, ModState}.

handoff_cancelled(ModState) ->
    {ok, ModState}.

handoff_finished(_Dest, ModState) ->
    {ok, ModState}.

handle_handoff_command(_Request, _Sender, ModState) ->
    {noreply, ModState}.

handle_handoff_data(_Data, ModState) ->
    {reply, ok, ModState}.

encode_handoff_item(_Key, _Value) ->
    corrupted.

is_empty(ModState) ->
    {true, ModState}.

terminate(_Reason, _ModState) ->
    ok.

delete(ModState) ->
    {ok, ModState}.
