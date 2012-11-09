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

-module(find_resource).
-author('ewan <ewan@ewansilver.com>').
-export([init/1, 
		 to_json/2,	
		 to_html/2,
		 content_types_provided/2]).
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
	{ok, undefined}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json},{"text/html", to_html}], RD, Ctx}.

to_html(ReqData, State) ->
	to_json(ReqData,State).


to_json(ReqData, State) ->
    QueryStrings = wrq:req_qs(ReqData),
    Matched_entries = matcher:find(QueryStrings),
    Size = length(Matched_entries),
    Output = rfc4627:encode({obj, [{"size", Size}, json_entry:encode_entries(Matched_entries)]}),
    {Output, ReqData, State}.