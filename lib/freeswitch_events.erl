%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This module is used to receive events from FreeSWITCH
%%  that are not directly tied to an interaction (for example registrations)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
-module(freeswitch_events).

-behaviour(gen_server).

-define(TIMEOUT, 5000).
-define(RESTART_TIME_INTERVAL, 3000).

%% API
-export([start/0, handle/1, handle_event/1, enter_event_loop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-define(HEARBEAT, 21000).

handle_event(Event) -> gen_server:cast(?MODULE, {handle_event, Event}).

start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% 
%% @doc Connect to freeswitch and register events.  Note that you have to do each 'CUSTOM' event seperately.

init([]) ->
	{ok, Node} = application:get_env(boss, freeswitch_node),
	spawn_link(?MODULE, enter_event_loop, [Node]),
	T = erlang:now(),
	{ok, {T, T}}.


handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

handle(Node) ->
	receive
		{event, [UUID | Event]} ->
			EName = freeswitch:get_event_name(Event),
			Subclass = freeswitch:get_event_header(Event, "Event-Subclass", undefined),
			FsEvent = fs_event:new(id, EName, Subclass, UUID, erlang:now(), Event),
			% io:format("FsEvent:  ~p~n", [FsEvent]),
			FsEvent:save(),
			handle(Node);
		A ->
			io:format("Recieved something else ~p~n", [A]),
			handle(Node)
	after 25000 ->
		io:format("No heartbeat from FreeSWITCH, reconnecting...~n"),
		enter_event_loop(Node)
	end.
	
%%
%% @doc Registers events with fs
register_events(Node) ->
	io:format("Connected! Register events to FreeSWITCH ...~n"),
	freeswitch:event(Node, [all]).

enter_event_loop(Node) ->
	io:format("Node: ~p~n", [Node]),
	monitor_node(Node, true),
	
	io:format("(re)Connecting to FreeSWITCH...~n"),

	{foo, Node} ! register_event_handler,
	receive
		ok ->
			register_events(Node),
			handle(Node);
		{error, Reason} ->
			io:format("freeswitch connector got error ~p", [Reason]),
			enter_event_loop(Node);
		{nodedown, Node} ->
			io:format("FreeSWITCH nodedown, restarting ..."),
			timer:sleep(?RESTART_TIME_INTERVAL),
			enter_event_loop(Node);
		X -> 
			io:format("FreeSWITCH connector got....~p", [X]),
			enter_event_loop(Node)
	after ?TIMEOUT ->
			io:format("Connecting to FreeSWITCH timeout..."),
			timer:sleep(?RESTART_TIME_INTERVAL),
			enter_event_loop(Node)
	end.
