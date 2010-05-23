<?php
require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . "Session.php");
require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . "Address.php");
require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . "Loop.php");

class MessagePackRPC_Client extends MessagePackRPC_Session
{
  public $addr = null;

  public function __construct($host, $port)
  {
    parent::__construct(new MessagePackRPC_Address($host, $port), new MessagePackRPC_Loop());
  }
  
  public function send($mt, $a)
  {
    return $this->sendingRequest($mt, $a);
  }

  /**
   * Method call synchronous
   *
   * @param string $mt Method Name
   * @param array  $a  Method Args
   *
   */
  public function call($mt, $a)
  {
    // TODO: Time out
    $fut = $this->send($mt, $a);
    $fut->join();
    $result = $fut->getResult();
    $errors = $fut->getErrors();

    if (!empty($errors)) {
      throw new Exception($errors);
    }

    return $result;
  }
}