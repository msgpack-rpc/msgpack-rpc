ERLC_FLAGS=-D UNITTEST +debug_info
# DEBUG=-DDEBUG
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
BEAMS=$(SOURCES:src/%.erl=ebin/%.beam)

ifndef ROOT
  ROOT=$(shell pwd)
endif
COMMON_TEST_LIB = $(shell erl -noshell -eval 'io:format("~s~n", [code:lib_dir(common_test)]).' -s init stop)
RUN_TEST_CMD = $(COMMON_TEST_LIB)/priv/bin/run_test

all:  build

build: $(BEAMS) 

ebin/%.beam: src/%.erl $(HEADERS) Makefile
	erlc -pa ebin $(ERLC_FLAGS) -o ebin/ $<

edoc: $(SOURCES)
	erl -noshell -eval 'ok=edoc:files(["src/tcadb.erl"], [{dir,"edoc"}]).' -s init stop
	@echo "  - building edoc done. see edoc/index.html"

test: build
	@echo "testing in ${ROOT} .."
	@mkdir -p test/log
	sh ${RUN_TEST_CMD} -dir ${ROOT}/test -logdir ${ROOT}/test/log \
	  -I ${ROOT}/include -I ${ROOT}/src  \
	  -cover ${ROOT}/test/yatce.coverspec  -pa ${ROOT}/ebin
	@echo "automated test done. see test/log/index.html."

#test: build
#	erl -noshell -pa ebin \
#	  -eval 'eunit:test("ebin", [verbose])' -s init stop

clean:
	-rm $(BEAMS)
