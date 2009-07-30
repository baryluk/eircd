-module(eircd_app).
-behaviour(application).

-export([start/2, stop/1, config_change/3]).

% TODO: failover, takeover
start(_Type, _Args) ->
	Ports = case application:get_env(eircd, listen_at_ports) of
		{ok, L} when list(L) ->
			L;
		undefined ->
			[6667]
	end,
	{ok, Pid} = eircd_sup:start_link(Ports),
	{ok, Pid}.

stop(_State) ->
	ok.

config_change(_Changed, _New, _Removed) ->
	ok.
