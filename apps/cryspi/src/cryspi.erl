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

basic_access_pattern() ->
    Goal = datalog_parse:parse("?-grandparent(zeus,GRANDCHILD)."),
    Goal2 = cryspi_syntax:add_depth(Goal, 0),
    cryspi_search:solve_goal(Goal2).
