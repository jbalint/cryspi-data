%% -------------------------------------------------------------------
%%
%%   cryspi_app: Application.
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

-module(cryspi_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    case cryspi_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register(cryspi, [{vnode_module, cryspi_search_vnode}]),
            ok = riak_core_node_watcher:service_up(cryspi, self()),
            {ok, Pid};
        Else -> Else
    end.

stop(_State) ->
    ok.
