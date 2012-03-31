package rpc

import (
	"reflect"
)

const (
	REQUEST      = 0
	RESPONSE     = 1
	NOTIFICATION = 2
)

type RPCError struct {
	Cause   interface{}
	Message string
}

type FunctionResolver interface {
	Resolve(name string, arguments []reflect.Value) (reflect.Value, error)
}

func (self *RPCError) Error() string {
	return self.Message
}
