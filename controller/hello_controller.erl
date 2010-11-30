-module(hello_controller, [Req]).
-compile(export_all).

world('GET', []) ->
    Greetings = boss_db:find('greeting', []),
    case length(Greetings) of
        0 -> 
            {ok, SavedGreeting} = (greeting:new(id, "Boss says Hello!")):save(),
            {ok, [{greeting, SavedGreeting}]};
        N -> {ok, [{greeting, lists:nth(random:uniform(N), Greetings)}]}
    end.


events('GET', []) ->
    Events = boss_db:find(fs_event, []),
    {ok, [{events, Events}]};

events('GET', [ID]) ->
    Events = boss_db:find(fs_event, []),
	Event = boss_db:find(ID),
    {ok, [{events, Events}, {event, Event}]}.

clear('GET', []) ->
	Events = boss_db:find(fs_event, []),
	lists:foreach(fun(Event) -> boss_db:delete(Event:id()) end, Events),
	{redirect, "/hello/events"}.
