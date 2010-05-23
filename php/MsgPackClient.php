<?php
require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . "MsgPackSession.php");
require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . "MsgPackAddress.php");
require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . "MsgPackLoop.php");

class MsgPackClient extends MsgPackSession
{
  public $addr = null;

  public function __construct($host, $port)
  {
    parent::__construct(new MsgPackAddress($host, $port), new MsgPackLoop());
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