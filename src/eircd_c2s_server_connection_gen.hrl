-record(client_state, {
	state=notlogged, % logged, quiting
	welcomed=no,
	reason=noreason, % quit reason
	username=undefined,
	realname=undefined,
	servername=undefined,
	hostname0=undefined,
	nick=undefined,
	fullnick=undefined, % nick!user@host
	hostaddr,
	hostname,
	port, % client side port
	channels,
	nickshandler=undefined
}).


%%-record(user,
%%	{name, % name
%%	fullname,
%%	ident,
%%	ip_address,
%%	is_local, % is it directly connected to us
%%	c2s_process, % if it is directly connected to us (any node)
%%	s2s_process, % if it is connected to some external server
%%	ip_tcp_port,
%%	flags,
%%	channels  % list of channel names and/or controling_processes
%%}).

%%-record(message,
%%	{type,
%%	from,
%%	to,
%%	text
%%}).
