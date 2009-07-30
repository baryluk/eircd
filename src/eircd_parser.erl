-module(eircd_parser).
-author('baryluk@smp.if.uj.edu.pl').

-export([parse_input/1]).

%% parser


-spec parse_input(binary()) -> list().

parse_input(<<":", _Rest/binary>>) ->
	badargs;

parse_input(<<"PRIVMSG ", Rest/binary>>) ->
	case split_fields(Rest) of
		[Receivers0, Text] ->
			Receivers = [binary_to_list(X) || X <- split_comma(Receivers0)],
			{privmsg, Receivers, Text}
	end;
parse_input(<<"JOIN ", Rest/binary>>) ->
	case split_fields(Rest) of
		[Channels] ->
			Channels2 = [binary_to_list(X) || X <- split_comma(Channels)],
			{join, Channels2, []};
		[Channels, Keys|_] ->
			Keys2 = [binary_to_list(X) || X <- split_comma(Keys)],
			Channels2 = [binary_to_list(X) || X <- split_comma(Channels)],
			{join, Channels2, Keys2}
	end;
parse_input(<<"PART ", Rest/binary>>) ->
	case split_fields(Rest) of
		[Channels] ->
			Channels2 = [binary_to_list(X) || X <- split_comma(Channels)],
			{part, Channels2, noreason};
		[Channels, Reason|_] ->
			Channels2 = [binary_to_list(X) || X <- split_comma(Channels)],
			{part, Channels2, Reason} % TODO: can be there reason?
	end;
parse_input(<<"TOPIC ", Rest/binary>>) ->
	case split_fields(Rest) of
		[Channel0, Topic] ->
			Channel = binary_to_list(Channel0),
			{topic, Channel, Topic};
		_ ->
			badargs
	end;
parse_input(<<"PING ", Rest/binary>>) ->
	case split_fields(Rest) of
		[Payload, Server2|_] ->
			{ping, Payload, Server2};
		[Payload] ->
			{ping, Payload};
		_ ->
			badargs
	end;
parse_input(<<"KICK ", Rest/binary>>) ->
	case split_fields(Rest) of
		[Channel, Nick, Reason] ->
			{kick, binary_to_list(Channel), binary_to_list(Nick), Reason};
		[Channel, Nick] ->
			{kick, binary_to_list(Channel), binary_to_list(Nick), noreason};
		_ ->
			badargs
	end;
parse_input(<<"NICK ", Nicks/binary>>) ->
	case split_fields(Nicks) of
		[Nick] ->
			{nick, binary_to_list(Nick)};
		_ ->
			badargs
	end;
	% `~|^_-[]\{}
	% :lublin.irc.pl 461 `` NICK :Not enough parameters
parse_input(<<"MODE ", Rest/binary>>) ->
	case split_fields(Rest) of
		[Nick] ->
			{mode, binary_to_list(Nick)};
		[Nick,Flags] ->
			{mode, binary_to_list(Nick), binary_to_list(Flags)};
		_ ->
			badargs
	end;
%	MODE baryluk
%	:lublin.irc.pl 403 alaaaaf baryluk :No such channel
%	MODE #debian +i
%	:lublin.irc.pl 482 alaaaaf #debian :You're not channel operator
%	MODE #debian
%	:lublin.irc.pl 324 alaaaaf #debian +tnl 
%	MODE #debian a a
%	:lublin.irc.pl 472 alaaaaf a :is unknown mode char to me for #debian
%	:lublin.irc.pl 472 alaaaaf a :is unknown mode char to me for #debian
%

parse_input(<<"WHO ", Rest/binary>>) ->
	case split_fields(Rest) of
		[Nick] ->
			{who, binary_to_list(Nick)};
		_ ->
			badargs
	end;

parse_input(<<"WHOIS ", Rest/binary>>) ->
	case split_fields(Rest) of
		[Nick] ->
			{whois, binary_to_list(Nick)};
		_ ->
			badargs
	end;

parse_input(<<"USER ", Rest/binary>>) ->
	case split_fields(Rest) of
		[UserName0, HostName0, ServerName0, RealName0] ->
			UserName = binary_to_list(UserName0),
			HostName = binary_to_list(HostName0),
			ServerName = binary_to_list(ServerName0),
			RealName = binary_to_list(RealName0),
			{user, UserName, HostName, ServerName, RealName};
		_ ->
			badargs
	end;
parse_input(<<"QUIT">>) ->
	{quit, noreason};
parse_input(<<"QUIT ", Rest/binary>>) ->
	[Reason] = split_fields(Rest),
	{quit, Reason};
parse_input(<<"LIST ", Rest/binary>>) ->
	[Filter] = split_fields(Rest),
	{list, Filter};
parse_input(<<"_EIRCD PING ", Rest/binary>>) ->
	{eircd, ping, Rest};
parse_input(<<"_EIRCD TRACE enable">>) ->
	{eircd, trace, enable};
parse_input(<<"_EIRCD TRACE disable">>) ->
	{eircd, trace, disable};
parse_input(<<"_EIRCD TIMEOUT ", Rest/binary>>) ->
	{eircd, timeout, list_to_integer(binary_to_list(Rest))};
parse_input(Line) ->
	{unknown, Line, split_fields(Line)}.

%% split comma

split_comma(Line) ->
	split_comma(Line, [], size(Line)).

split_comma(<<>>, A, 0) ->
	lists:reverse(A);
split_comma(Line, A, Length) ->
	L = search_char($,, Line),
	case L of
		_N when _N < Length ->
			<<Left:L/binary, ",", Right/binary>> = Line,
			split_comma(Right, [Left|A], Length-L-1);
		_ ->
			split_comma(<<>>, [Line|A], Length-L)
	end.

search_char(Char, Line) when is_integer(Char) ->
	search_char(Char, Line, 0).

search_char(_Char, <<>>, N) ->
	N;
search_char(Char, <<Char:8, _Rest/binary>>, N) ->
	N;
search_char(Char, <<_:8, Rest/binary>>, N) ->
	search_char(Char, Rest, N+1).

%% split_fields

split_fields(Line) when binary(Line) ->
	split_fields(Line, []).

split_fields(<<>>, Acc) ->
	lists:reverse(Acc);
split_fields(<<":", Rest/binary>>, Acc) ->
	lists:reverse([Rest|Acc]);
split_fields(Line, Acc) ->
	{L, R} = split_once_pos(Line),
	split_fields(R, [L | Acc]).

split_once_pos(Line) ->
	{Lp, Rest1} = split_once_pos(Line, 0),
	{_Mp, Right} = split_once_pos2(Rest1, 0),
	<<Left:Lp/binary, _Rest2/binary>> = Line,
	{Left, Right}.

split_once_pos(<<>>, LAcc) ->
	{LAcc, <<>>};
split_once_pos(<<$\s, RestLine/binary>>, LAcc) ->
	{LAcc, RestLine};
split_once_pos(<<_Byte:8, RestLine/binary>>, LAcc) ->
	split_once_pos(RestLine, LAcc+1).

split_once_pos2(<<>>, MAcc) ->
	{MAcc, <<>>};
split_once_pos2(<<$\s, RestLine/binary>>, MAcc) ->
	split_once_pos2(RestLine, MAcc+1);
split_once_pos2(<<_Byte:8, _RestLine/binary>> = Line, MAcc) ->
	{MAcc, Line}.
