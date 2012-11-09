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

-module(json_entry).
-author('ewan <ewan@ewansilver.com>').
%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([decode/1,
		 decode_lease/1,
		 encode/3,
		 encode_entry/3,
		 encode_entries/1,
		 encode_links/1,
		 encode_registration/2]).

%%
%% API Functions
%%

%% Decode the JSON request.
%% Return {Keys,Data,Lease} where Keys =[{Name,Value}]
decode(JSON) ->
	{struct,[{"entry",{struct,Body}}]} = mochijson:decode(JSON),
	Keys = case proplists:get_value("keys",Body) of
			   {struct,Key} -> {ok,Key};
			   _ -> {undefined}
				end,
	Data = case proplists:get_value("data",Body) of
			   undefined -> {undefined};
			   List -> {ok,list_to_binary(List)}
				end,
	Lease = case proplists:get_value("lease",Body) of
			   undefined -> {undefined};
			   V -> {ok,V}
				end,
	{Keys,Data,Lease}.

%% Decode a JSON lease request. Just used for renewing an entity.
decode_lease(JSON) ->
	{struct,[{"lease",Lease}]} = mochijson:decode(JSON),
	Lease.

encode(Keys,Data,Lease) -> Entry = encode_entry(Keys,Data,Lease),
						   rfc4627:encode({obj,[Entry]}).
encode_entry(Keys,Data,Lease) -> {entry, {obj, [{keys, {obj, encode_keys(Keys)}}, {data, Data}, {lease, Lease}]}}.
encode_entries(Entries) -> {entries, {obj,  [json_entry:encode_entry(Keys, Data, Lease) || {Keys, Data, Lease} <- Entries]}}.

%% Encode a links relationship. 
%% Links are of the form {"links":{[rel:href]} where rel signifies the relationship of the link and href represents the link location.
encode_links(Links) ->
	rfc4627:encode({obj,[create_links(Links)]}).
	
create_links(Links) ->
	{links, {obj, encode_keys(Links)}}.

%% Encode the JSON response for the register_resource creation call.
%% It is of the format: {"lease":lease_period, "links":{[rel:href]}}
encode_registration(Lease,Links) ->
	JSON = {obj,[{lease,Lease},create_links(Links)]},
	rfc4627:encode(JSON).

%%
%% Local Functions
%%

%% rfc4627 expects the value to be a binary. We need to ensure that they are suitably encoded
encode_keys(Keys) ->
	encode(Keys,[]).
encode([],Acc)-> Acc;
encode([{Name,Value}|Tail],Acc) ->
	encode(Tail,[{Name,encode(Value)}|Acc]).
encode(Value) when is_binary(Value) -> Value;
encode(Value) when is_list(Value) -> list_to_binary(Value);
encode(Value) -> term_to_binary(Value).

%% Tests
encode_decode_test() ->
	Keys=[{"one","1"},{"two","2"}],
	Data = <<"Some Data">>,
	Lease=123456789,
	JSON = json_entry:encode(Keys,Data,Lease),
	{{ok,Keys1},{ok,Data},{ok,Lease}} = json_entry:decode(JSON),
	Keys = lists:sort(Keys1)	.
