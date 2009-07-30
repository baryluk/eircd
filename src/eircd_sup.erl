-module(eircd_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Ports) when is_list(Ports) ->
	supervisor:start_link({local, eircd_sup}, eircd_sup, Ports).

init(Ports) when is_list(Ports) ->
	{ok, {{one_for_one, 1, 60},
		lists:map(fun (Port) -> 
				Id = list_to_atom("eircd_c2s_server_sup_" ++ integer_to_list(Port)),
				{Id, {eircd_c2s_server_sup, start_link, [Port]},
					permanent, infinity, supervisor, [eircd_c2s_server_sup]}
			end,
			Ports)
		++
		[{eircd_logger, {logger, start_link, []},
			permanent, 10000, worker, [logger]},
		{eircd_nicks, {eircd_nicks, start_link, []},
			permanent, 10000, worker, [eircd_nicks]},
		{eircd_channel_sup, {eircd_channel_sup, start_link, []},
			permanent, infinity, supervisor, [eircd_channel_sup]}]
	}}.
