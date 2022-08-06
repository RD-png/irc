REBAR=rebar3
ERL=erl

compile:
	${REBAR} compile

clean:
	@rm -f rebar.lock
	@rm -rf _build
	${REBAR} clean

dev:
	${REBAR} shell --config config/sys.config

deps:
	${REBAR} tree

dialyzer:
	${REBAR} dialyzer
