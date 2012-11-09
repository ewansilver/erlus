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

-module(entity_resource).
-author('ewan <ewan@ewansilver.com>').
-export([init/1, 
		 to_json/2,	
		 to_html/2,
		 json_put/2,
		 content_types_provided/2,
		 content_types_accepted/2,
		 resource_exists/2,
		 delete_resource/2,
		 allowed_methods/2]).
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
	{ok, undefined}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json},{"text/html", to_html}], RD, Ctx}.
content_types_accepted(RD, Ctx) ->
    {[{"application/json", json_put}], RD, Ctx}.

resource_exists(ReqData,_State) ->
	ID = wrq:disp_path(ReqData),
	case matcher:find({id,ID}) of
		{} ->    {false, ReqData, resource_not_exist};
		{Keys,Data,Lease} -> {true, ReqData,{Keys,Data,Lease,ID}}
	end.

allowed_methods(ReqData, State) ->
	{['GET','DELETE','PUT'], ReqData, State}.

json_put(ReqData, {Keys,Data,_Lease,ID}) ->
	Body = wrq:req_body(ReqData),
	Lease = json_entry:decode_lease(Body),
	case matcher:update_lease(ID,Lease) of
		{fail} -> {false, ReqData, {Keys,Data,Lease,ID}};
		{K,D,L} -> {true, wrq:set_resp_body(json_entry:encode(K, D, L), ReqData), {K,D,L,ID}}
	end;
json_put(ReqData, resource_not_exist) ->
	{false, ReqData, resource_not_exist}.

to_html(ReqData, State) ->
	to_json(ReqData,State).

to_json(ReqData, {Keys,Data,Lease,ID}) ->
	{json_entry:encode(Keys, Data, Lease), ReqData, {Keys,Data,Lease,ID}}.

delete_resource(ReqData, {Keys,Data,Lease,ID}) ->
	{ok} = matcher:delete(ID),
	{true, ReqData,{Keys,Data,Lease,ID}}.