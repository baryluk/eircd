-record(channel_flags,
	{a=false,	% annonymous
	i=false,	% invite
	m=false,	% moderated
	n=true,	% no messages from outside
	q=false,	% quiet
	p=false,	% private
	s=false,	% secret
	r=false,	% reop
	t=true,	% topic settable only by operators
	k="",	% key (password)
	l=40,	% user limit
	b=[],	% ban mask list
	e=[],	% exception mask list
	'I'=[]	% invation mask list
}).

-record(channel_state,
	{name=unknown,
	topic=undefined,
	topic_changer=undefined,
	topic_changed_at=undefined,
	owner=unknown,
	flags=#channel_flags{},
	local_nicks=[],
	nonlocal_nicks=[],
	controling_hub=unknown,
	members_nicks,			% dict(): nick() -> pid()
	members_pids,			% dict(): pid() -> [nick()]
	members_with_op=[],
	members_with_voice=[]
}).
