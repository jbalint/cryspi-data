%% -------------------------------------------------------------------
%%
%%   cryspi_sup: Supervisor.
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

-module(cryspi_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

% start supervisor linked to caller (application)
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    % http://www.erlang.org/doc/man/supervisor.html#type-child_spec
    %% SearchVnode = #{id => cryspi_search_vnode,
    %%                 start => {riak_core_vnode_master, start_link, [cryspi_search_vnode]}},
    SearchVnode = {cryspi_search_vnode,
                   {riak_core_vnode_master, start_link, [cryspi_search_vnode]},
                   permanent, 5000, worker, [riak_core_vnode_master]},
    {ok, {%#{strategy => one_for_one}, % all default supervisor flags are fine
       {one_for_one, 1, 5},
       [SearchVnode]}}.
