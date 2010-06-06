ERLC_FLAGS=-D UNITTEST +debug_info
# DEBUG=-DDEBUG
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
BEAMS=$(SOURCES:src/%.erl=ebin/%.beam)

all:  build

build: $(BEAMS) 

ebin/%.beam: src/%.erl $(HEADERS) Makefile
	erlc -pa ebin $(ERLC_FLAGS) -o ebin/ $<

test: build
	erl -noshell -pa ebin \
	  -eval 'eunit:test("ebin", [verbose])' -s init stop

clean:
	-rm $(BEAMS)
