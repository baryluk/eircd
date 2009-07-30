-module(logger_file).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

init(File) ->
	{ok, Fd} = file:open(File, write),
	{ok, Fd}.

handle_event(ErrorMsg, Fd) ->
	io:format(Fd, "***Error*** ~p~n", [ErrorMsg]),
	{ok, Fd}.

handle_call(_Request, State) ->
	io:format("logger_terminal received unknown call: ~p~n", [Request]),
	{ok, {unknown_call, make_ref()}, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Args, Fd) ->
	file:close(Fd),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
