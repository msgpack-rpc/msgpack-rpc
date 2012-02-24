package rpc

import (
	"fmt"
	"io"
	"msgpack"
	"net"
	"os"
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
		return nil, &Error{err, "Failed to send a request message"}
	}
	_msgId, result, _err := ReceiveResponse(self.conn.(io.Reader))
	if err != nil {
		return nil, _err
	}
	if msgId != _msgId {
		return nil, &Error{nil, fmt.Sprintf("Message IDs don't match (%d != %d)", msgId, _msgId)}
	}
	if self.autoCoercing {
		_result, ok := result.(reflect.ArrayOrSliceValue)
		if ok {
			elemType, ok := _result.Type().(reflect.ArrayOrSliceType).Elem().(*reflect.UintType)
			if ok && elemType.Kind() == reflect.Uint8 {
				result = reflect.NewValue(string(_result.Interface().([]byte)))
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
func SendRequestMessage(writer io.Writer, msgId int, funcName string, arguments []interface{}) os.Error {
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
	_, err = msgpack.PackArray(writer, reflect.NewValue(arguments).(reflect.ArrayOrSliceValue))
	return err
}

// This is a low-level function that is not supposed to be called directly
// by the user.  Change this if the MessagePack protocol is updated.
func ReceiveResponse(reader io.Reader) (int, reflect.Value, *Error) {
	data, _, err := msgpack.UnpackReflected(reader)
	if err != nil {
		return 0, nil, &Error{nil, "Error occurred while receiving a response"}
	}

	msgId, result, _err := HandleRPCResponse(data)
	if _err != nil {
		return 0, nil, _err
	}
	return msgId, result, nil
}

// This is a low-level function that is not supposed to be called directly
// by the user.  Change this if the MessagePack protocol is updated.
func HandleRPCResponse(req reflect.Value) (int, reflect.Value, *Error) {
	_req, ok := req.Interface().([]reflect.Value)
	if !ok {
		goto err
	}
	if len(_req) != 4 {
		goto err
	}
	msgType, ok := _req[0].(*reflect.IntValue)
	if !ok {
		goto err
	}
	msgId, ok := _req[1].(*reflect.IntValue)
	if !ok {
		goto err
	}
	if _req[2] != nil {
		_errorMsg, ok := _req[2].(reflect.ArrayOrSliceValue)
		if ok {
			errorMsg, ok := _errorMsg.Interface().([]uint8)
			if !ok {
				goto err
			}
			if msgType.Get() != RESPONSE {
				goto err
			}
			if errorMsg != nil {
				return int(msgId.Get()), nil, &Error{nil, string(errorMsg)}
			}
		} else {
			goto err
		}
	}
	return int(msgId.Get()), _req[3], nil
err:
	return 0, nil, &Error{nil, "Invalid message format"}
}
