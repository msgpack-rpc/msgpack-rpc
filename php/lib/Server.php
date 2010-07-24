<?php
class MessagePackRPC_Server
{
  public $back = null;
  public $hand = null;

  public function __construct($back, $hand)
  {
    $this->back = $back;
    $this->hand = $hand;
  }

  public function recv($data)
  {
    try {
      list($code, $func, $args) = $this->back->serverRecvObject($data);
      $hand = $this->hand;
      $recv = call_user_func_array(array($hand, $func), $args);
      return $this->back->serverSendObject($code, $recv, "");
    } catch (Exception $e) {
      $errs = $e->getMessage();
      return $this->back->serverSendObject($code, "", $errs);
    }
  }
}