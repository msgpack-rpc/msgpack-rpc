ERLC_FLAGS=-D UNITTEST
# DEBUG=-DDEBUG
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)

all: $(EBIN_FILES) build

build: $(OBJECTS) 

ebin/%.beam: src/%.erl $(HEADERS) watchdog Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

test: build
	erl -noshell -pa ebin \
	  -eval 'eunit:test("ebin", [verbose])' -s init stop

clean:
	-rm $(OBJECTS)
