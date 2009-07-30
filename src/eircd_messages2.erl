-module(eircd_messages2).
-author('baryluk@smp.if.uj.edu.pl').

-compile([export_all]).

handle_msg_msg(ChannelName, FromFullNick, Text) ->
	[":", FromFullNick, <<" PRIVMSG ">>, ChannelName, <<" :">>, Text, crlf].

handle_msg_join(ChannelName, WhoFullNick) ->
	[":", WhoFullNick, <<" JOIN :">>, ChannelName, crlf].

handle_msg_part(ChannelName, WhoFullNick, Reason) ->
	Reason2 = case Reason of
		noreason ->
			"";
		RealReason when is_binary(RealReason) ->
			[" :", RealReason];
		_ ->
			<<" :complicated eircd structure">>
	end,
	[":", WhoFullNick, <<" PART ">>, ChannelName, Reason2, crlf].

handle_msg_kick(ChannelName, WhoNick, Reason, ByNick) ->
	[":", eircd_messages:fqdn(), <<" KICK ">>, ChannelName, " ", WhoNick, " ", ByNick, <<" :">>, Reason, crlf].

handle_msg_nick(OldFullNick, NewNick) ->
	[":", OldFullNick, <<" NICK :">>, NewNick, crlf].

handle_msg_topic(ChannelName, ByFullNick, NewTopic) ->
	[":", ByFullNick, <<" TOPIC ">>, ChannelName, <<" :">>, NewTopic, crlf].

handle_msg_stop(_ChannelName) ->
	[].

handle_msg_members(_ChannelName) ->
	[].

handle_msg_mode(ChannelName, ByFullNick, Who, M) ->
	[":", ByFullNick, <<" MODE ">>, ChannelName, M, Who, crlf].

handle_msg_oped(ChannelName, ByFullNick, Who) ->
	handle_msg_mode(ChannelName, ByFullNick, Who, <<" +o ">>).
handle_msg_deoped(ChannelName, ByFullNick, Who) ->
	handle_msg_mode(ChannelName, ByFullNick, Who, <<" -o ">>).

handle_msg_voiced(ChannelName, ByFullNick, Who) ->
	handle_msg_mode(ChannelName, ByFullNick, Who, <<" +v ">>).
handle_msg_devoiced(ChannelName, ByFullNick, Who) ->
	handle_msg_mode(ChannelName, ByFullNick, Who, <<" -v ">>).
