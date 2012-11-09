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

-module(entrance_resource).
-author('ewan <ewan@ewansilver.com>').
-export([init/1,
         content_types_provided/2,
         to_json/2,
		 to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json},{"text/html", to_html}], RD, Ctx}.

to_html(RD, State) ->
	to_json(RD, State).

to_json(RD, State) ->
	Port=8000,
	Register = utils:create_links(register,Port,"/register/"),
	Find  =	utils:create_links(find,Port,"/find/"),
	Links = lists:append(Register,Find),
	JSONBody = json_entry:encode_links(Links),
    {true, wrq:append_to_response_body(JSONBody, RD), State}.


