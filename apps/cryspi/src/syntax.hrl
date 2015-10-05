%% -------------------------------------------------------------------
%%
%%   syntax: Syntactic types use throughout the system.
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

-type node_type() :: rule | disj | conj | func | const | literal | goal.

-type func_node() :: {func, string(), [term_node()]}.
-type const_node() :: {const, string()}.
-type literal_node() :: {literal, integer()}.
-type var_node() :: {var, string()}.
% @doc Rule is a conjunction of atomic formulas which satisfy the head
% (atomic) formula.
-type rule_node() :: {rule, func_node(), [func_node()]}.

-type term_node() :: func_node() | const_node() | literal_node() | var_node().

-type unifier() :: #{string() => term_node()}.
