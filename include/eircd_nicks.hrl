-record(nicks_state,
	{register_nicks,	% dict(): nick() -> pid()
	register_pids		% dict(): pid() -> [nick()]
}).
