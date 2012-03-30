package rpc

import (
	"fmt"
	"io"
	"msgpack"
	"net"
	"reflect"
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

// Sends a RPC request to the server.
func (self *Session) SendV(funcName string, arguments []interface{}) (reflect.Value, *Error) {
	var msgId = self.nextId
	self.nextId += 1
	if self.autoCoercing {
		arguments = coerce(arguments)
	}
	err := SendRequestMessage(self.conn.(io.Writer), msgId, funcName, arguments)
	if err != nil {
		return reflect.Value{}, &Error{err, "Failed to send a request message"}
	}
	_msgId, result, _err := ReceiveResponse(self.conn.(io.Reader))
	if err != nil {
		return reflect.Value{}, _err
	}
	if msgId != _msgId {
		return reflect.Value{}, &Error{nil, fmt.Sprintf("Message IDs don't match (%d != %d)", msgId, _msgId)}
	}
	if self.autoCoercing {
		_result := result
		if _result.Kind() == reflect.Array || _result.Kind() == reflect.Slice {
			elemType := _result.Type().Elem()
			if (elemType.Kind() == reflect.Uint || elemType.Kind() == reflect.Uint8 || elemType.Kind() == reflect.Uint16 || elemType.Kind() == reflect.Uint32 || elemType.Kind() == reflect.Uint64 || elemType.Kind() == reflect.Uintptr) &&
				elemType.Kind() == reflect.Uint8 {
				result = reflect.ValueOf(string(_result.Interface().([]byte)))
			}
		}
	}
	return result, nil
}

// Sends a RPC request to the server.
func (self *Session) Send(funcName string, arguments ...interface{}) (reflect.Value, *Error) {
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
func ReceiveResponse(reader io.Reader) (int, reflect.Value, *Error) {
	data, _, err := msgpack.UnpackReflected(reader)
	if err != nil {
		return 0, reflect.Value{}, &Error{nil, "Error occurred while receiving a response"}
	}

	msgId, result, _err := HandleRPCResponse(data)
	if _err != nil {
		return 0, reflect.Value{}, _err
	}
	return msgId, result, nil
}

// This is a low-level function that is not supposed to be called directly
// by the user.  Change this if the MessagePack protocol is updated.
func HandleRPCResponse(req reflect.Value) (int, reflect.Value, *Error) {
	_req, ok := req.Interface().([]reflect.Value)
	if !ok {
		return 0, reflect.Value{}, &Error{nil, "Invalid message format"}
	}
	if len(_req) != 4 {
		return 0, reflect.Value{}, &Error{nil, "Invalid message format"}
	}
	msgType := _req[0]
	typeOk := msgType.Kind() == reflect.Int || msgType.Kind() == reflect.Int8 || msgType.Kind() == reflect.Int16 || msgType.Kind() == reflect.Int32 || msgType.Kind() == reflect.Int64
	if !typeOk {
		return 0, reflect.Value{}, &Error{nil, "Invalid message format"}
	}
	msgId := _req[1]
	if msgId.Kind() != reflect.Int && msgId.Kind() != reflect.Int8 && msgId.Kind() != reflect.Int16 && msgId.Kind() != reflect.Int32 && msgId.Kind() != reflect.Int64 {
		return 0, reflect.Value{}, &Error{nil, "Invalid message format"}
	}
	if _req[2].IsValid() {
		_errorMsg := _req[2]
		if _errorMsg.Kind() == reflect.Array || _errorMsg.Kind() == reflect.Slice {
			errorMsg, ok := _errorMsg.Interface().([]uint8)
			if !ok {
				return 0, reflect.Value{}, &Error{nil, "Invalid message format"}
			}
			if msgType.Int() != RESPONSE {
				return 0, reflect.Value{}, &Error{nil, "Invalid message format"}
			}
			if errorMsg != nil {
				return int(msgId.Int()), reflect.Value{}, &Error{nil, string(errorMsg)}
			}
		} else {
			return 0, reflect.Value{}, &Error{nil, "Invalid message format"}
		}
	}
	return int(msgId.Int()), _req[3], nil
}
