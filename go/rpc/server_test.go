package msgpack_rpc_test

import (
	. "msgpack/rpc"
	"testing"
	"net"
	"reflect"
)

type Resolver map[string]reflect.Value

func (self Resolver) Resolve(name string, arguments []reflect.Value) (reflect.Value, *Error) {
	return self[name], nil
}

func echo(test string) (string, interface {
	String() string
}) {
	return "Hello, " + test, nil
}

func TestRun(t *testing.T) {
	res := Resolver{"echo": reflect.ValueOf(echo)}
	serv := NewServer(res, true, nil)
	l, err := net.Listen("tcp", "127.0.0.1:50000")
	if err != nil {
		t.Fail()
		return
	}
	serv.Listen(l)
	go (func() { serv.Run() })()
	conn, err := net.Dial("tcp", "127.0.0.1:50000")
	if err != nil {
		t.Fail()
		return
	}
	client := NewSession(conn, true)
	for _, v := range []string{"world", "test", "hey"} {
		retval, xerr := client.Send("echo", v)
		if xerr != nil {
			t.Error(xerr.String())
			return
		}
		_retval := retval
		if _retval.Kind() != reflect.String {
			return
		}
		if _retval.String() != "Hello, "+v {
			t.Error("retval != \"Hello, " + v + "\"")
		}
	}
}
