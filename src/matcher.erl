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

-module(matcher).
-author('ewan <ewan@ewansilver.com>').
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([find/1,
		 register/3,
		 delete/1,
		 update_lease/2,
		 start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {entries=[],
						max_lease=15*60*1000 %% Max lease we accept is set to 15 mins.
			   }).
-record(entry,{keys,
			   			data,
			   			id,
			   			lease, 	 %% LeaseTime in ms
			   			registered_time}).

%% ====================================================================
%% External functions
%% ====================================================================
start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% List of keys
register(Template,Data,Lease) ->
	gen_server:call(?MODULE, {register,{Template,Data,Lease}}).

update_lease(ID,Lease) when is_list(ID) ->
	update_lease(list_to_binary(ID),Lease);
update_lease(ID,Lease) ->
		gen_server:call(?MODULE, {update_lease,ID,Lease}).

delete(ID) when is_list(ID) ->
	delete(list_to_binary(ID));
delete(ID) ->
		gen_server:call(?MODULE, {delete,ID}).

find({id,ID}) when is_list(ID) ->
	find({id,list_to_binary(ID)});
find({id,ID}) ->
		gen_server:call(?MODULE, {get,ID});
find(Template) ->
	gen_server:call(?MODULE, {find,Template}).
%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{entries=[]}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------
handle_call({delete,ID}, _From, State) ->
    Entries = lists:dropwhile(fun(Entry) -> Entry#entry.id == ID end,remove_timed_out_leases(State#state.entries)),
    {reply, {ok}, State#state{entries=Entries}};

handle_call({update_lease,ID,RequestedLease}, _From, State) ->
	Update_fun = fun(Entry,Result) -> 
						 case Entry#entry.id == ID of
							 false -> {Entry,Result};
							 true -> NewEntry = Entry#entry{registered_time = erlang:now(),
															lease = allocated_lease(RequestedLease,State#state.max_lease)},
									 {NewEntry,NewEntry}
						 end
						 end,
								 
    {Entries,UpdatedEntry} = lists:mapfoldl(Update_fun,[],remove_timed_out_leases(State#state.entries)),
	Result = case UpdatedEntry of
				 [] -> {fail};
				 _ ->{UpdatedEntry#entry.keys,UpdatedEntry#entry.data,remaining_lease(UpdatedEntry)}
				 end,
				 
    {reply, Result, State#state{entries=Entries}};

handle_call({get,ID}, _From, State) ->
	Entries = remove_timed_out_leases(State#state.entries),
    Reply = case lists:filter(fun(Entry) -> Entry#entry.id == ID end,Entries) of
				[] -> {};
				[Entry] -> {Entry#entry.keys,Entry#entry.data,remaining_lease(Entry)}
			end,
    {reply, Reply, State#state{entries=Entries}};

handle_call({find,Template}, _From, State) ->
	Entries = remove_timed_out_leases(State#state.entries),
    Reply = lists:filter(fun({_,_,Lease}) -> Lease >0 end,[ {Entry#entry.keys,Entry#entry.data,remaining_lease(Entry)}||Entry <- match(Template,Entries)]),
    {reply, Reply, State#state{entries=Entries}};

handle_call({register,{Template,Data,Lease}}, _From, State) ->
	ID = id_generator:next(),
	LeaseTime=allocated_lease(Lease,State#state.max_lease),
	Entry = #entry{data=Data,
				   keys=lists:sort(Template),
				   id=ID,
				   lease=LeaseTime,
				   registered_time=erlang:now()},
    Reply = {ok,LeaseTime,ID},
    {reply, Reply, State#state{entries=[Entry|remove_timed_out_leases(State#state.entries)]}}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


%% Find the entries that match the supplied Template.
%% Entries = [#entry]
%% Templates and Entries are expected to be ordered by ascending name
match([],Entries) -> Entries;
match(_Template,[]) -> [];
match([Head|Tail], Entries) ->
	match(Tail,lists:filter(fun(Entry) -> Keys = Entry#entry.keys, 
									lists:member(Head,Keys) end,Entries)).

remove_timed_out_leases(Entries) ->
	 lists:filter(fun(Entry) -> remaining_lease(Entry)> 0 end,Entries).

remaining_lease(Entry) ->
    RemainingLease = erlang:trunc(Entry#entry.lease - timer:now_diff(erlang:now(), Entry#entry.registered_time) / 1000),
    case RemainingLease > 0 of
      true -> RemainingLease;
      _ -> 0
    end.

allocated_lease(Lease,Max_lease) when Lease > Max_lease -> Max_lease;
allocated_lease(Lease,_Max_lease)  -> Lease.
  
%% Tests %%

fail_test() ->
	E1 = #entry{keys= [{two,2},{three,3}]},
	E2 = #entry{keys= [{a,a}]},
	[] = match([{one,1}],[E1,E2]).

basic_pass_test() ->
	E1 = #entry{keys= [{two,2},{three,3}]},
	E2 = #entry{keys= [{a,a}]},
	[E1] = match([{two,2}],[E1,E2]).

string_pass_test() ->
	E1 = #entry{keys= [{"two","2"},{"three","3"}]},
	E2 = #entry{keys= [{"a","a"}]},
	[E1] = match([{"two","2"}],[E1,E2]).

empty_entries_list_test() ->
	[] = match([{"two","2"}],[]).

multiple_entries_test() ->
	E1 = #entry{keys= [{two,2},{three,3}]},
	E2 = #entry{keys= [{a,a}]},
	E3 = #entry{keys=[{four,4},{two,2}]},
	[E1,E3] = match([{two,2}],[E1,E2,E3]).