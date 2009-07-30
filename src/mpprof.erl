-module(mpprof).
-author('baryluk@smp.if.uj.edu.pl').

% This module is for message passign tracing and profiling
% There is a internal mechanisms to this, in BEAM machine.
% http://www.erlang.org/doc/man/seq_trace.html
% todo: parametrized module

-record(msg,
	{msg, % term
	origintag, % term, same as last element in history
	originnow, % now(), same as last element in history
	history % list()
	}).

% Message passing profiling and tracing

-export([start/1,
	start/2,
	extract/1,
	addtag/2,
	like/3,
	send/2,
	merge/4]).

% Starts profiled message

% Start profilling message for Msg if not started already
start({mpprof_msg_v1, _, _, _, _} = Msg) ->
	Msg;
start(Msg) ->
	start(Msg, start).

% Start profilling message with tag OriginTag
start(Msg, OriginTag) ->
	OriginNow = now(),
	{mpprof_msg_v1, Msg, OriginTag, OriginNow, [{OriginTag, OriginNow}]}.

% Extract orginal message (works for proffiling dissable and enabled
extract({mpprof_msg_v1, Msg, _, _, _}) ->
	Msg;
extract(Msg) ->
	Msg.

% Extract profile (works for proffiling dissable and enabled)
profile({mpprof_msg_v1, _, _, _, Profile}) ->
	Profile;
profile(_Msg) ->
	[].

% Add tag to profilled messaged, if not already profilled it is started
addtag(NewTag, {mpprof_msg_v1, Msg, OriginTag, OriginNow, List}) ->
	NewNow = now(),
	{mpprof_msg_v1, Msg, OriginTag, OriginNow, [{NewTag, NewNow}|List]};
addtag(NewTag, Msg) ->
	addtag(NewTag, start(Msg, mpprof_auto)).

% Create new message NewMsg with tag NewTag, and tags from OrMessage
like(NewMsg, NewTag, {mpprof_msg_v1, _OldMsg, OriginTag, OriginNow, List}) ->
	NewNow = now(),
	{mpprof_msg_v1, NewMsg, OriginTag, OriginNow, [{NewTag, NewNow}|List]}.

% Send message Msg to Pid
send(Pid, Msg) ->
	Pid ! addtag({mpprof_send, self()}, Msg).

% Merge profilles of Messages M1 and M2 and create new NewMsg with NewTag containing this profilles
% Good for profilling gether/scatter, barriers, waiting for other processes
merge(NewMsg, NewTag, M1, M2) ->
	start(NewMsg, {NewTag, merge, profile(M1), profile(M2)}).
