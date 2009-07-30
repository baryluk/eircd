{application, eircd,
	[{description, "Distributed Fault-tolerant and High Performance Erlang Internet Relay Chat Server"},
	{vsn, "0.0.0.1"},
	{modules, [eircd_app, eircd_sup,
		eircd_c2s_server_sup, eircd_c2s_server_gen,
		eircd_c2s_server_connection_sup, eircd_c2s_server_connection_gen,
		eircd_channel_sup, eircd_channel,
		eircd_nicks,
		gen_tcp_server, gen_tcp_server_connection,
		eircd_messages, eircd_messages2,
		eircd_parser,
		logger, logger_file, logger_terminal,
		mpprof
	]},
	{registered, [eircd]},
	{applications, [kernel, stdlib, sasl]},
	{mod, {eircd_app, []}},
	{env, [{listen_at_ports, [6667, 6668, 7776]}, {logfile, "eircd.log"}]}
	]
}.
