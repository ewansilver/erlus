%% @author Ewan Silver <ewan@ewansilver.com>
%% @copyright 2010 Ewan Silver
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.
-module(id_generator_sup).
-author('ewan <ewan@ewansilver.com>').
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------
-export([
		 start_link/1,
		 init/1
        ]).

%% ====================================================================
%% External functions
%% ====================================================================

%% @spec start_link(SecretKey) -> ServerRet
%% @doc API for starting the supervisor.
start_link(SecretKey) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [SecretKey]).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([SecretKey]) ->
    AChild = {'id_generator',{'id_generator',start,[SecretKey]},
	      permanent,2000,worker,['id_generator']},
    {ok,{{one_for_all,0,1}, [AChild]}}.
