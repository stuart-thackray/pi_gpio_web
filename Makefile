REBAR:=./rebar

all:
ifeq ("$(wildcard rebar.config)","")
	@(echo "No backend specified. Defaulting to inets")
	@($(MAKE) inets)
else
	@($(MAKE) get-deps compile copy-static)
endif

help:
	@(echo)
	@(echo "Build ")
	@(echo)
	@(echo "   make [cowboy|inets|mochiweb|webmachine|yaws]")
	@(echo)
	@(echo "Execute")
	@(echo)
	@(echo "   make run")
	@(echo)

get-deps:
	$(REBAR) get-deps

update-deps:
	$(REBAR) update-deps

compile:
	$(REBAR) compile

link-static:
	(cd static; rm -fr nitrogen; ln -s ../deps/nitrogen_core/www nitrogen)
	(cd static; rm -fr doc; ln -s ../deps/nitrogen_core/doc/html doc)

copy-static:
	(cd static; rm -rf nitrogen; mkdir nitrogen; cp -r ../deps/nitrogen_core/www/* nitrogen)
##	(cd static; cp -r ../deps/bootstrap/dist/js/* js ; cp -r ../deps/bootstrap/dist/css/* css)
##	(cd static; rm -rf fonts ; mkdir fonts ; cp -r ../deps/bootstrap/dist/fonts/* fonts )
##	(cd static; cp -r ../deps/fa/css/* css ; cp -r ../deps/fa/fonts/* fonts)
##	(cd static; rm -rf doc; mkdir doc; cp -r ../deps/nitrogen_core/doc/html/* doc)

clean:
	$(REBAR) clean

cowboy:
	@($(MAKE) platform PLATFORM=cowboy)

inets:
	@($(MAKE) platform PLATFORM=inets)

mochiweb:
	rm -fr deps/mochiweb
	@($(MAKE) platform PLATFORM=mochiweb)

webmachine:
	rm -fr deps/mochiweb
	@($(MAKE) platform PLATFORM=webmachine)

yaws:
	@($(MAKE) platform PLATFORM=yaws)

platform:
	@(echo "Fetching initial dependencies...")
	($(REBAR) --config rebar.base.config get-deps)
	@(deps/simple_bridge/rebar_deps/merge_deps.escript rebar.base.config deps/simple_bridge/rebar_deps/$(PLATFORM).deps rebar.config)
	@(echo "Updating app.config...")
	@(sed 's/{backend, [a-z]*}/{backend, $(PLATFORM)}/' < app.config > app.config.temp)
	@(mv app.config.temp app.config)
	@($(MAKE) get-deps compile copy-static)


upgrade: update-deps compile copy-static

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib sasl crypto compiler syntax_tools

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo 
	@(dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS) -r ./deps/)

dialyzer: mochiweb $(DEPS_PLT)
	@(dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin)

travis: dialyzer

TESTLOG:=testlog.log

run:
	@(echo "You might be requested for sudo password to access GPIO pins")
	sudo erl -pa ebin ./deps/*/ebin ./deps/*/include \
	-config "app.config" \
	-sname gpio_web \
	-env ERL_FULLSWEEP_AFTER 0 \
	-eval "inets:start()" \
	-eval "application:start(pi_gpio_web_interface)."


