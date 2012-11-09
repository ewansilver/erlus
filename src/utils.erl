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

%% A set of utility functions.

-module(utils).
-author('ewan <ewan@ewansilver.com>').
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_interfaces/1,
		 create_links/3]).

%%
%% API Functions
%%

%% This uses an undocumented function inet:getif/0 to find out the list of IPs that webmachine is listening on and then 
%% uses them as the basis with which to generate a list of URLs that the entry can be accessed via. 
get_interfaces(Relationship) ->
	{ok,IPs} = inet:getif(),
	lists:map(fun({{A,B,C,D},_,_}) -> {Relationship,string:join([integer_to_list(Entry)||Entry<- tuple_to_list({A,B,C,D})],".")} end,strip_out_localhost(IPs)).


%% Create a set of urls with the appropriate relation tags.
%% Returns -> [{relationship,URL}]
create_urls( Hosts,Port,Path) ->
	[{Relationship,string:join(["http://",Host,":",integer_to_list(Port),Path],"")}|| {Relationship,Host}<-Hosts].

%% Create a list of links of the format {relationship,uri} 
create_links(Relationship,Port,Path) ->
	create_urls(utils:get_interfaces(Relationship),Port,Path).

%%
%% Local Functions
%%


%% If there is more than one IP address available then strip out the localhost/127.0.0.1 entry if it is present.
%% If the localhost entry is the only one available then return it because we are obviously working on a dev box.
%% All this inet:getif/0 stuff is a hack and there must be a better way to find out the IPs that a webmachine instance
%% is running on :(
strip_out_localhost([{{127,0,0,1},_,_}]=IP) -> IP;
strip_out_localhost(IPs) -> lists:filter(fun({IP,_,_}) -> not ({127,0,0,1}== IP) end, IPs).
