APP=memcached
LIBS=$(ERL_LIBS):deps

compile:
	rebar compile

start: compile
	@ERL_LIBS=$(LIBS) erl -pa ebin -eval 'application:ensure_started(${APP}).'

eunit: compile
	rebar eunit skip_deps=true
