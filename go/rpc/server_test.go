package rpc

import (
	"fmt"
	"net"
	"reflect"
	"testing"
)

type Resolver map[string]reflect.Value

func (self Resolver) Resolve(name string, arguments []reflect.Value) (reflect.Value, error) {
	return self[name], nil
}

func echo(test string) (string, fmt.Stringer) {
	return "Hello, " + test, nil
}

func add(a, b uint) (uint, fmt.Stringer) {
	return a + b, nil
}

func TestRun(t *testing.T) {
	res := Resolver{"echo": reflect.ValueOf(echo), "add": reflect.ValueOf(add)}
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
			t.Error(xerr)
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

	for _, v := range []struct{ a, b, c int }{{0, 0, 0}, {1, 1, 2}, {2, 3, 5}, {31337, 0x0eadbeef, 31337 + 0x0eadbeef}} {
		retval, xerr := client.Send("add", v.a, v.b)
		if xerr != nil {
			t.Error(xerr)
			continue
		}
		i := CoerceInt(retval)
		if i != int64(v.c) {
			t.Errorf("add: got %d expected %d\n", i, int64(v.c))
		}
	}
}
