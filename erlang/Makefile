ERLC_FLAGS=-D UNITTEST +debug_info
# DEBUG=-DDEBUG
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
BEAMS=$(SOURCES:src/%.erl=ebin/%.beam)

ifndef ROOT
  ROOT=$(shell pwd)
endif

all:  build

build: $(BEAMS) 

ebin/%.beam: src/%.erl $(HEADERS) Makefile
	erlc -pa ebin $(ERLC_FLAGS) -o ebin/ $<

doc: $(SOURCES) $(HEADERS)
	erl -noshell -eval \
	 'ok=edoc:files(["src/msgpack.erl", src/mp_session.erl", "src/mp_client.erl"], [{dir,"edoc"}]).'\
	 -s init stop
	@echo "  - building edoc done. see edoc/index.html"

test: build
	@echo "testing in ${ROOT} .."
	@mkdir -p test/log
	erl -sname ct -s ct_run script_start -s erlang halt \
	  -dir $(ROOT)/test -logdir $(ROOT)/test/log \
	  -I $(ROOT)/include -I $(ROOT)/src -cover $(ROOT)/test/my.coverspec  -pa $(ROOT)/ebin
	@echo "automated test done. see test/log/index.html."

install: build
	./scripts/setup.es install

uninstall:
	./scripts/setup.es uninstall

clean:
	-rm $(BEAMS)
