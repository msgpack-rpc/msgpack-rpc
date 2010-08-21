<?php
class MessagePackRPC_Back
{
  public $errorMessage01 = 'Network error';
  public $errorMessage02 = 'Objects error';
  public $size;

  public function __construct($size = 1024)
  {
    $this->size = $size;
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
    $sock = fsockopen($host, $port);
    if ($sock === FALSE) throw new Exception($this->errorMessage01);
    $puts = fputs($sock, $call);
    if ($puts === FALSE) throw new Exception($this->errorMessage01);
    $read = fread($sock, $size);
    if ($read === FALSE) throw new Exception($this->errorMessage01);
    $end = fclose($sock);

    return $read;
  }

  public function clientRecvObject($recv)
  {
    $data = $this->msgpackDecode($recv);

    $type = $data[0];
    $code = $data[1];
    $sets = $data[2];
    $errs = $data[3];

    if ($type != 1) {
      throw new Exception($this->errorMessage02);
    }

    $feature = new MessagePackRPC_Future();
    $feature->setErrors($errs);
    $feature->setResult($sets);

    return $feature;
  }

  public function serverSendObject($code, $errs, $sets)
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
      throw new Exception($this->errorMessage02);
    }

    $type = $data[0];
    $code = $data[1];
    $func = $data[2];
    $args = $data[3];

    if ($type != 0) {
      throw new Exception($this->errorMessage02);
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