%% Author: ewan
%% Created: 28 Sep 2010
%% Description: Manages hypermedia links. All resources in a REST application can be driven by a set of hypermedia links.
%% They typically contain an HREF which points to the next URI to be interacted with and a RELATIONSHIP which specifies
%% what kind of link the HREF is pointing to.

%% @type rel() = string(). A Relationship.
%% @type href() = string(). A URI
%% @type link() = {href(),rel()}. A link.
%% @type links() = [link()]. A list of link()s.

-module(links).
-author('ewan <ewan@ewansilver.com>').
%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([encode/2,
		 encode/3,
		 decode/2]).

%%
%% API Functions
%%

%% Encodes a json formatted link relationship. It is of the format: {href:"Href()" rel:"Rel()"}
encode(json,Href,Rel) -> 
	rfc4627:encode(json_obj(Href,Rel)).

%% Encode the supplied Links() as Json
%% Links = links() 
%% Result json of the format {links:[{"href":Href(),"rel":rel()}]}
encode(json,Links) ->
	A = [json_obj(Href,Rel) || {Href,Rel} <- Links],
	rfc4627:encode({obj, [{"links", A}]}).

%%decode(json,Json_string) when is_list(Json_string)->
%%	decode(json,list_to_binary(Json_string));
decode(json,Json_string) ->
	{ok,{obj,Struct},[]} = rfc4627:decode(Json_string),
	case Struct of
		[{"href",Href},{"rel",Rel}] -> {binary_to_list(Href),binary_to_list(Rel)};
		[{"links",Links}] -> [{binary_to_list(Href),binary_to_list(Rel)} || {obj,[{"href",Href},{"rel",Rel}]} <-Links]
	end.
  
%%
%% Local Functions
%%
%% The value in a json tuple must be a binary.
%% returns {Key,Value} where Value is binary encoded.
json_obj(Href,Rel) ->
	{obj,[json_tuple("href",Href),json_tuple("rel",Rel)]}.
json_tuple(Key,Value) when is_list(Value) ->
	json_tuple(Key,list_to_binary(Value));
json_tuple(Key,Value) ->
	{Key,Value}.

%% TESTS

single_json_link_test() ->
	Rel = "jump",
	Href = "http://how.high.com",
	JSON = encode(json,Href,Rel),
	{Href,Rel} = decode(json,JSON).

json_links_test() ->
	Links = [{"H1","R1"},{"H2","R2"}],
	JSON = encode(json,Links),
	Links = decode(json,JSON).