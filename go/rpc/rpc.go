package rpc

import (
	"reflect"
)

const (
	REQUEST      = 0
	RESPONSE     = 1
	NOTIFICATION = 2
)

type Error struct {
	Cause   interface{}
	Message string
}

type FunctionResolver interface {
	Resolve(name string, arguments []reflect.Value) (*reflect.FuncValue, *Error)
}

func (self *Error) String() string {
	return self.Message
}
