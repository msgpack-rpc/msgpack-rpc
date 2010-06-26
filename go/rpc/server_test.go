package msgpack_rpc_test

import (
    . "msgpack/rpc"
    "testing"
    "net"
    "reflect"
)

type Resolver map[string] *reflect.FuncValue

func (self Resolver) Resolve(name string, arguments []reflect.Value) (*reflect.FuncValue, *Error) {
    return self[name], nil
}

func echo(test string) (string, interface{ String() string }) {
    return "Hello, " + test, nil
}

func TestRun(t *testing.T) {
    res := Resolver{ "echo": reflect.NewValue(echo).(*reflect.FuncValue) }
    serv := NewServer(res, true, nil)
    l, err := net.Listen("tcp", "127.0.0.1:50000")
    if err != nil {
        t.Fail()
        return
    }
    serv.Listen(l)
    go (func() { serv.Run() })()
    conn, err := net.Dial("tcp", "", "127.0.0.1:50000")
    if err != nil {
        t.Fail()
        return
    }
    client := NewSession(conn, true)
    retval, xerr := client.Send("echo", "test")
    if xerr != nil {
        t.Error(xerr.String())
        return
    }
    _retval, ok := retval.(*reflect.StringValue)
    if !ok {
        return
    }
    if _retval.Get() != "Hello, test" { t.Error("retval != \"Hello, test\"") }

}
