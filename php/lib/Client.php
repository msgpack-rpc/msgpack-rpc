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
    $code    = 0;
    $call    = $this->back->clientCallObject($func, $args, $code);
    $send    = $this->back->clientConnection($host, $port, $call);
    $feature = $this->back->clientRecvObject($send);

    $result  = $feature->getResult();
    $errors  = $feature->getErrors();

    if (0 < count($errors)) {
      throw new Exception($errors);
    }

    return $result;
  }

  public function call($func, $args)
  {
    return $this->send($func, $args);
  }
}