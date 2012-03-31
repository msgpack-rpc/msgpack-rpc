<?php

class MessagePackRPC_Error extends Exception
{
  function __construct($msg = '', $code = null, $prev = null) {
    if (is_array($msg) && array_key_exists('message', $msg))
      $msg = $msg['message'];
    parent::__construct($msg, $code, $prev);
  }
}
class MessagePackRPC_Error_NetworkError extends MessagePackRPC_Error {}
class MessagePackRPC_Error_ProtocolError extends MessagePackRPC_Error {}
class MessagePackRPC_Error_RequestError extends MessagePackRPC_Error {}


