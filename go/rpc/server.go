package rpc

import (
	"container/list"
	"fmt"
	"io"
	"log"
	msgpack "github.com/msgpack/msgpack/go"
	"net"
	"os"
	"reflect"
	"errors"
)


type Server struct {
	resolver     FunctionResolver
	log          *log.Logger
	listeners    list.List
	autoCoercing bool
	lchan        chan int
}

func onList(l list.List, on func(interface{})) {
	for e := l.Front(); e != nil; e = e.Next() {
		on(e.Value)
	}
}

// Goes into the event loop to get ready to serve.
func (self *Server) Run() *Server {
	lchan := make(chan int)
	onList(self.listeners, func(listener interface{}) {
		_listener := listener.(net.Listener)
		go (func() {
			for {
				conn, err := _listener.Accept()
				if err != nil {
					self.log.Println(err)
					continue
				}
				if self.lchan == nil {
					conn.Close()
					break
				}
				go (func() {
					for {
						data, _, err := msgpack.UnpackReflected(conn)
						if err == io.EOF {
							break
						} else if err != nil {
							self.log.Println(err)
							break
						}
						msgId, funcName, _arguments, xerr := HandleRPCRequest(data)
						if xerr != nil {
							self.log.Println(xerr)
							continue
						}
						f, xerr := self.resolver.Resolve(funcName, _arguments)
						if xerr != nil {
							self.log.Println(xerr)
							SendErrorResponseMessage(conn, msgId, xerr.Error())
							continue
						}
						funcType := f.Type()
						if funcType.NumIn() != len(_arguments) {
							msg := fmt.Sprintf("The number of the given arguments (%d) doesn't match the arity (%d)", len(_arguments), funcType.NumIn())
							self.log.Println(msg)
							SendErrorResponseMessage(conn, msgId, msg)
							continue
						}
						if funcType.NumOut() != 1 && funcType.NumOut() != 2 {
							self.log.Println("The number of return values must be 1 or 2")
							SendErrorResponseMessage(conn, msgId, "Internal server error")
							continue
						}
						var arguments []reflect.Value
						if self.autoCoercing {
							arguments = make([]reflect.Value, funcType.NumIn())
							for i, v := range _arguments {
								ft := funcType.In(i)
								vt := v.Type()
								if ft == vt {
									arguments[i] = v
								} else {
									if ft != nil {
										_vt := v.Type()
										if _vt.Kind() == reflect.Array || _vt.Kind() == reflect.Slice {
											et := _vt.Elem()
											if et != nil && et.Kind() == reflect.Uint8 {
												arguments[i] = reflect.ValueOf(string(v.Interface().([]byte)))
												continue
											}
										}
									}
									msg := fmt.Sprintf("The type of argument #%d doesn't match (%s expected, got %s)", i, ft.String(), vt.String())
									self.log.Println(msg)
									SendErrorResponseMessage(conn, msgId, msg)
									continue
								}
							}
						} else {
							for i, v := range _arguments {
								ft := funcType.In(i)
								vt := v.Type()
								if ft != vt {
									msg := fmt.Sprintf("The type of argument #%d doesn't match (%s expected, got %s)", i, ft.String(), vt.String())
									self.log.Println(msg)
									SendErrorResponseMessage(conn, msgId, msg)
									continue
								}
							}
							arguments = _arguments
						}
						retvals := f.Call(arguments)
						if funcType.NumOut() == 2 {
							var errMsg fmt.Stringer = nil
							var ok bool
							_errMsg := retvals[1].Interface()
							if _errMsg != nil {
								errMsg, ok = _errMsg.(fmt.Stringer)
								if !ok {
									self.log.Println("The second argument must have an interface { String() string }")
									SendErrorResponseMessage(conn, msgId, "Internal server error")
									continue
								}
							}
							if errMsg == nil {
								if self.autoCoercing {
									_retval := retvals[0]
									if _retval.Kind() == reflect.String {
										retvals[0] = reflect.ValueOf([]byte(_retval.String()))
									}
								}
								SendResponseMessage(conn, msgId, retvals[0])
							} else {
								SendErrorResponseMessage(conn, msgId, errMsg.String())
							}
						} else {
							SendResponseMessage(conn, msgId, retvals[0])
						}
					}
					conn.Close()
				})()
			}
		})()
	})
	self.lchan = lchan
	<-lchan
	onList(self.listeners, func(listener interface{}) {
		listener.(net.Listener).Close()
	})
	return self
}

// Lets the server quit the event loop
func (self *Server) Stop() *Server {
	if self.lchan != nil {
		lchan := self.lchan
		self.lchan = nil
		lchan <- 1
	}
	return self
}

// Listenes on the specified transport.  A single server can listen on the
// multiple ports.
func (self *Server) Listen(listener net.Listener) *Server {
	self.listeners.PushBack(listener)
	return self
}

// Creates a new Server instance. raw bytesc are automatically converted into
// strings if autoCoercing is enabled.
func NewServer(resolver FunctionResolver, autoCoercing bool, _log *log.Logger) *Server {
	if _log == nil {
		_log = log.New(os.Stderr, "msgpack: ", log.Ldate|log.Ltime)
	}
	return &Server{resolver, _log, list.List{}, autoCoercing, nil}
}

// This is a low-level function that is not supposed to be called directly
// by the user.  Change this if the MessagePack protocol is updated.
func HandleRPCRequest(req reflect.Value) (int, string, []reflect.Value, error) {
	for{
		_req, ok := req.Interface().([]reflect.Value)
		if !ok {
			break;
		}
		if len(_req) != 4 {
			break;
		}
		msgType := _req[0]
		typeOk := msgType.Kind() == reflect.Int || msgType.Kind() == reflect.Int8 || msgType.Kind() == reflect.Int16 || msgType.Kind() == reflect.Int32 || msgType.Kind() == reflect.Int64
		if !typeOk {
			break;
		}
		msgId := _req[1]
		idOk := msgId.Kind() == reflect.Int || msgId.Kind() == reflect.Int8 || msgId.Kind() == reflect.Int16 || msgId.Kind() == reflect.Int32 || msgId.Kind() == reflect.Int64
		if !idOk {
			break;
		}
		_funcName := _req[2]
		funcOk := _funcName.Kind() == reflect.Array || _funcName.Kind() == reflect.Slice
		if !funcOk {
			break;
		}
		funcName, ok := _funcName.Interface().([]uint8)
		if !ok {
			break;
		}
		if msgType.Int() != REQUEST {
			break;
		}
		_arguments := _req[3]
		var arguments []reflect.Value
		if _arguments.Kind() == reflect.Array || _arguments.Kind() == reflect.Slice {
			elemType := _req[3].Type().Elem()
			_elemType := elemType
			ok := _elemType.Kind() == reflect.Uint || _elemType.Kind() == reflect.Uint8 || _elemType.Kind() == reflect.Uint16 || _elemType.Kind() == reflect.Uint32 || _elemType.Kind() == reflect.Uint64 || _elemType.Kind() == reflect.Uintptr
			if !ok || _elemType.Kind() != reflect.Uint8 {
				arguments, ok = _arguments.Interface().([]reflect.Value)
			} else {
				arguments = []reflect.Value{reflect.ValueOf(string(_req[3].Interface().([]byte)))}
			}
		} else {
			arguments = []reflect.Value{_req[3]}
		}
		return int(msgId.Int()), string(funcName), arguments, nil
	}
	return 0, "", nil, errors.New("Invalid message format")
}

// This is a low-level function that is not supposed to be called directly
// by the user.  Change this if the MessagePack protocol is updated.
func SendResponseMessage(writer io.Writer, msgId int, value reflect.Value) error {
	_, err := writer.Write([]byte{0x94})
	if err != nil {
		return err
	}
	_, err = msgpack.PackInt8(writer, RESPONSE)
	if err != nil {
		return err
	}
	_, err = msgpack.PackInt(writer, msgId)
	if err != nil {
		return err
	}
	_, err = msgpack.PackNil(writer)
	if err != nil {
		return err
	}
	_, err = msgpack.PackValue(writer, value)
	return err
}

// This is a low-level function that is not supposed to be called directly
// by the user.  Change this if the MessagePack protocol is updated.
func SendErrorResponseMessage(writer io.Writer, msgId int, errMsg string) error {
	_, err := writer.Write([]byte{0x94})
	if err != nil {
		return err
	}
	_, err = msgpack.PackInt8(writer, RESPONSE)
	if err != nil {
		return err
	}
	_, err = msgpack.PackInt(writer, msgId)
	if err != nil {
		return err
	}
	_, err = msgpack.PackBytes(writer, []byte(errMsg))
	if err != nil {
		return err
	}
	_, err = msgpack.PackNil(writer)
	return err
}
