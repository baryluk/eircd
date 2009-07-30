-module(eircd_channel_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_channel/2]).

start_link() ->
	supervisor:start_link({local, eircd_channel_sup}, eircd_channel_sup, []).

init(_Args) ->
	{ok, {{simple_one_for_one, 0, 1},
		[{channel, {eircd_channel, start_link, []},
			temporary, 2000, worker, [eircd_channel]}]}}.

start_channel(ChannelName, Owner) ->
	supervisor:start_child(eircd_channel_sup, [ChannelName, Owner]).
