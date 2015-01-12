all: deps compile

compile: deps
	./rebar compile

deps:
	test -d deps || ./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps