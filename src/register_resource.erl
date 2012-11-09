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

-module(register_resource).
-author('ewan <ewan@ewansilver.com>').
-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
%% expecting to get the data in a JSON pasted form
%% keys = the keys,
%%data = the binary payload.
init(_) -> {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

process_post(RD, Ctx) ->
    Body = wrq:req_body(RD),
    {{ok, Keys}, {ok, Data}, {ok, Lease}} = json_entry:decode(Body),
    {ok, LeaseTime, ID} = matcher:register(Keys, Data, Lease),
	URLs = utils:create_links(self,8000,string:concat("/entity/", binary_to_list(ID))),
    JSONBody =json_entry:encode_registration(LeaseTime,URLs),
    {true, wrq:append_to_response_body(JSONBody, RD), Ctx}.
	