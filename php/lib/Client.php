<?php
include_once dirname(__FILE__) . '/Back.php';

class MessagePackRPC_Client
{
  public $back = null;
  public $host = null;
  public $port = null;

  public function __construct($host, $port, $back = null)
  {
    $this->back = $back == null ? new MessagePackRPC_Back() : $back;
    $this->host = $host;
    $this->port = $port;
  }

  public function send($func, $args)
  {
    $host    = $this->host;
    $port    = $this->port;
    $code    = 0;
    $call    = $this->back->clientCallObject($code, $func, $args);
    $send    = $this->back->clientConnection($host, $port, $call);
    $feature = $this->back->clientRecvObject($send);

    $result  = $feature->getResult();
    $errors  = $feature->getErrors();

    if (0 < strlen($errors)) {
      throw new Exception($errors);
    }

    return $result;
  }

  public function call($func, $args)
  {
    return $this->send($func, $args);
  }
}