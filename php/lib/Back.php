<?php
require_once dirname(__FILE__) . '/Future.php';

class MessagePackRPC_Back
{
  public $size;
  public static $shared_client_socket = null;
  public $client_socket = null;
  public $use_shared_connection = true;
  public $reuse_connection = true;

  public function __construct($size = 1024, $opts = array())
  {
    $this->size = $size;
    if (array_key_exists('reuse_connection', $opts))
      $this->reuse_connection = $opts['reuse_connection'];
    if (array_key_exists('use_shared_connection', $opts))
      $this->use_shared_connection = $opts['use_shared_connection'];
  }

  public function __destruct()
  {
    if (self::$shared_client_socket)
      fclose(self::$shared_client_socket);
    if ($this->client_socket)
      fclose($this->client_socket);
  }

  public function clientCallObject($code, $func, $args)
  {
    $data    = array();
    $data[0] = 0;
    $data[1] = $code;
    $data[2] = $func;
    $data[3] = $args;

    return $data;
  }

  public function clientConnection($host, $port, $call)
  {
    $size = $this->size;
    $send = $this->msgpackEncode($call);
    $sock = $this->connect($host, $port);
    if ($sock === FALSE) throw new MessagePackRPC_Error_NetworkError(error_get_last());
    $puts = fputs($sock, $send);
    if ($puts === FALSE) throw new MessagePackRPC_Error_NetworkError(error_get_last());
    $read = fread($sock, $size);
    if ($read === FALSE) throw new MessagePackRPC_Error_NetworkError(error_get_last());
    if (!$this->reuse_connection)
      fclose($sock);

    return $read;
  }

  public function connect($host, $port) {
    if (!$this->reuse_connection)
      return fsockopen($host, $port);
    $sock = $this->use_shared_connection ? self::$shared_client_socket : $this->client_socket;
    if ($sock && !feof($sock))
      return $sock;
    if (!$sock) {
        $sock = fsockopen($host, $port);
    } elseif (feof($sock)) {
        $sock = fsockopen($host, $port);
    }
    if ($this->use_shared_connection) {
      self::$shared_client_socket = $sock;
    } else {
      $this->client_socket = $sock;
    }
    return $sock;
  }

  public function clientRecvObject($recv)
  {
    $data = $this->msgpackDecode($recv);

    $type = $data[0];
    $code = $data[1];
    $errs = $data[2];
    $sets = $data[3];

    if ($type != 1) {
      throw new MessagePackRPC_Error_ProtocolError("Invalid message type for response: {$type}");
    }

    $feature = new MessagePackRPC_Future();
    $feature->setErrors($errs);
    $feature->setResult($sets);

    return $feature;
  }

  public function serverSendObject($code, $sets, $errs)
  {
    $data    = array();
    $data[0] = 1;
    $data[1] = $code;
    $data[2] = $errs;
    $data[3] = $sets;

    $send = $this->msgpackEncode($data);

    return $send;
  }

  public function serverRecvObject($recv)
  {
    $data = $this->msgpackDecode($recv);

    if (count($data) != 4) {
      throw new MessagePackRPC_Error_ProtocolError("Invalid message structure.");
    }

    $type = $data[0];
    $code = $data[1];
    $func = $data[2];
    $args = $data[3];

    if ($type != 0) {
      throw new MessagePackRPC_Error_ProtocolError("Invalid message type for request: {$type}");
    }

    return array($code, $func, $args);
  }

  public function msgpackDecode($data)
  {
    return msgpack_unpack($data);
  }

  public function msgpackEncode($data)
  {
    return   msgpack_pack($data);
  }
}
