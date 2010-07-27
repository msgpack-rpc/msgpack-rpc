<?php
class MessagePackRPC_Client
{
  public $back = null;
  public $host = null;
  public $port = null;

  public function __construct($back, $host, $port)
  {
    $this->back = $back;
    $this->host = $host;
    $this->port = $port;
  }

  public function send($call)
  {
    $host    = $this->host;
    $port    = $this->port;
    $req     = $this->back->clientCallObject($func, $args);
    $res     = $this->back->clientConnection($host, $port, $req);
    $feature = $this->back->clientRecvObject($res);

    $result  = $feature->getResult();
    $errors  = $feature->getErrors();

    if (0 < count($errors)) {
      throw new Exception($errors);
    }

    return $result;
  }

  public function call($func, $args)
  {
    $feature = $this->send($func, $args);
  }
}