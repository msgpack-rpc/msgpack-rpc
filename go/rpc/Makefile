include $(GOROOT)/src/Make.inc

TARG=msgpack/rpc

GOFILES=rpc.go server.go client.go

include $(GOROOT)/src/Make.pkg

%: install %.go
	$(GC) $*.go
	$(LD) -o $@ $*.$O
