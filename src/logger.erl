-module(logger).

-export([start_link/0]).

start_link() ->
	start_link(application:get_env(eircd, logfile)).

start_link(undefined) ->
	start_link("eircd.log");
start_link(Filename) ->
	{ok, Pid} = gen_event:start_link({local, eircd_logger}),
	gen_event:start({local, eircd_logger}),
	gen_event:add_handler(eircd_logger, logger_terminal, []),
	gen_event:add_handler(eircd_logger, logger_file, Filename),
	{ok, Pid}.
