-module(logger_terminal).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

init(_Args) ->
	{ok, []}.

handle_event(ErrorMsg, State) ->
	io:format("***Error*** ~p~n", [ErrorMsg]),
	{ok, State}.

handle_call(Request, State) ->
	io:format("logger_terminal received unknown call: ~p~n", [Request]),
	{ok, {unknown_call, make_ref(), State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Args, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
