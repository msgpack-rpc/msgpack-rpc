package rpc

import (
	"reflect"
)

const (
	REQUEST      = 0
	RESPONSE     = 1
	NOTIFICATION = 2
)

type FunctionResolver interface {
	Resolve(name string, arguments []reflect.Value) (reflect.Value, error)
}
