<?php
class MessagePackRPC_Address
{
  public $host = null;
  public $port = null;

  public function __construct($host, $port)
  {
    $this->host = $host;
    $this->port = $port;
  }

  public function getHost()
  {
    return $this->host;
  }

  public function getPort()
  {
    return $this->port;
  }
}