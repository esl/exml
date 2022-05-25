.PHONY: rel deps test

REBARVER = 3.15.2
ifeq ($(OTPVER),24.3)
	REBARVER = 3.17.0
endif
ifeq ($(OTPVER),25.0)
	REBARVER = 3.18.0
endif

all: deps compile

compile: rebar3
	./rebar3 compile

deps: rebar3
	./rebar3 get-deps

clean: rebar3
	./rebar3 clean && rm -f c_src/exml.gc*

test-deps: rebar3
	./rebar3 get-deps

test-compile: rebar3 test-deps
	./rebar3 as test compile

test: test-compile
	./rebar3 eunit

coverage-report: _build/test/cover/eunit.coverdata
	./rebar3 as test coveralls send

codecov: _build/test/cover/eunit.coverdata
	./rebar3 as test codecov analyze

gcov: test-compile
	gcov -o c_src exml

rebar3:
	wget https://github.com/erlang/rebar3/releases/download/${REBARVER}/rebar3 &&\
	chmod u+x rebar3

dialyzer: rebar3
	./rebar3 dialyzer
