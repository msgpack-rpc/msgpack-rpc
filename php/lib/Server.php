<?php
class MessagePackRPC_Server
{
  public $port = null;
  public $back = null;
  public $hand = null;

  public function __construct($port, $hand, $back = null)
  {
    $this->back = $back == null ? new MessagePackRPC_Back() : $back;
    $this->port = $port;
    $this->hand = $hand;
  }

  public function recv()
  {
    try {
      $sockItem = socket_create_listen($this->port);
      $sockList = array($sockItem);

      if ($sockItem === FALSE) {
        throw new Exception(); // TODO:
      }

      // TODO : Server connection check
      // TODO : Server connection outer
      while (TRUE) {
        $moveList = $sockList;
        $moveNums = socket_select($moveList, $w = null, $e = null, null);
        foreach ($moveList as $moveItem) {
          if ($moveItem == $sockItem) {
            $acptItem   = socket_accept($sockItem);
            $sockList[] = $acptItem;
          } else {
            $data = socket_read($moveItem, $this->back->size);

            list($code, $func, $args) = $this->back->serverRecvObject($data);
            $hand = $this->hand;
            $send = $this->back->serverSendObject($code, call_user_func_array(array($hand, $func), $args), "");
            socket_write($moveItem, $send);

            $move = unset($sockList[array_search($moveItem, $sockList)]);
            socket_close($moveItem);
          }
        }
      }

      socket_close($sockItem);
    } catch (Exception $e) {
      // TODO:
    }
  }
}