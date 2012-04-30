package rpc

import (
	"fmt"
	"io"
	msgpack "github.com/msgpack/msgpack-go"
	"net"
	"reflect"
	"errors"
)

type Session struct {
	conn         net.Conn
	autoCoercing bool
	nextId       int
}

func coerce(arguments []interface{}) []interface{} {
	_arguments := make([]interface{}, len(arguments))
	for i, v := range arguments {
		switch _v := v.(type) {
		case string:
			_arguments[i] = []byte(_v)
		default:
			_arguments[i] = _v
		}
	}
	return _arguments
}


// CoerceInt takes a reflected value and returns it as an int64
// panics if not an integer type
func CoerceInt(v reflect.Value) int64 {
	if isIntType(v) {
		return v.Int()
	}

	if isUintType(v) {
		return int64(v.Uint())
	}

	panic("not integer type")
}

// CoerceUint takes a reflected value and returns it as an uint64
// panics if not an integer type
func CoerceUint(v reflect.Value) uint64 {

	if isUintType(v) {
		return v.Uint()
	}

	if isIntType(v) {
		return uint64(v.Int())
	}

	panic("not integer type")
}

// Sends a RPC request to the server.
func (self *Session) SendV(funcName string, arguments []interface{}) (reflect.Value, error) {
	var msgId = self.nextId
	self.nextId += 1
	if self.autoCoercing {
		arguments = coerce(arguments)
	}
	err := SendRequestMessage(self.conn.(io.Writer), msgId, funcName, arguments)
	if err != nil {
		return reflect.Value{}, errors.New("Failed to send a request message: " + err.Error())
	}
	_msgId, result, err := ReceiveResponse(self.conn.(io.Reader))
	if err != nil {
		return reflect.Value{}, err
	}
	if msgId != _msgId {
		return reflect.Value{}, errors.New(fmt.Sprintf("Message IDs don't match (%d != %d)", msgId, _msgId))
	}
	if self.autoCoercing {
		_result := result
		if _result.Kind() == reflect.Array || _result.Kind() == reflect.Slice {
			elemType := _result.Type().Elem()
			if elemType.Kind() == reflect.Uint8 {
				result = reflect.ValueOf(string(_result.Interface().([]byte)))
			}
		}
	}
	return result, nil
}

// Sends a RPC request to the server.
func (self *Session) Send(funcName string, arguments ...interface{}) (reflect.Value, error) {
	return self.SendV(funcName, arguments)
}

// Creates a new session with the specified connection.  Strings are
// automatically converted into raw bytes if autoCoercing is
// enabled.
func NewSession(conn net.Conn, autoCoercing bool) *Session {
	return &Session{conn, autoCoercing, 1}
}

// This is a low-level function that is not supposed to be called directly
// by the user.  Change this if the MessagePack protocol is updated.
func SendRequestMessage(writer io.Writer, msgId int, funcName string, arguments []interface{}) error {
	_, err := writer.Write([]byte{0x94})
	if err != nil {
		return err
	}
	_, err = msgpack.PackInt(writer, REQUEST)
	if err != nil {
		return err
	}
	_, err = msgpack.PackInt(writer, msgId)
	if err != nil {
		return err
	}
	_, err = msgpack.PackBytes(writer, []byte(funcName))
	if err != nil {
		return err
	}
	_, err = msgpack.PackArray(writer, reflect.ValueOf(arguments))
	return err
}

// This is a low-level function that is not supposed to be called directly
// by the user.  Change this if the MessagePack protocol is updated.
func ReceiveResponse(reader io.Reader) (int, reflect.Value, error) {
	data, _, err := msgpack.UnpackReflected(reader)
	if err != nil {
		return 0, reflect.Value{}, errors.New("Error occurred while receiving a response")
	}

	msgId, result, err := HandleRPCResponse(data)
	if err != nil {
		return 0, reflect.Value{}, err
	}
	return msgId, result, nil
}

// This is a low-level function that is not supposed to be called directly
// by the user.  Change this if the MessagePack protocol is updated.
func HandleRPCResponse(req reflect.Value) (int, reflect.Value, error) {
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
		if msgId.Kind() != reflect.Int && msgId.Kind() != reflect.Int8 && msgId.Kind() != reflect.Int16 && msgId.Kind() != reflect.Int32 && msgId.Kind() != reflect.Int64 {
			break;
		}
		if _req[2].IsValid() {
			_errorMsg := _req[2]
			if _errorMsg.Kind() != reflect.Array && _errorMsg.Kind() != reflect.Slice {
                            break;
                        }
                        errorMsg, ok := _errorMsg.Interface().([]uint8)
                        if !ok {
                                break;
                        }
                        if msgType.Int() != RESPONSE {
                                break;
                        }
                        if errorMsg != nil {
				return int(msgId.Int()), reflect.Value{}, errors.New(string(errorMsg))
			}
		}
		return int(msgId.Int()), _req[3], nil
	}
	return 0, reflect.Value{}, errors.New("Invalid message format")
}
