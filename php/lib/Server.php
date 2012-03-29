<?php
require_once dirname(__FILE__) . '/Back.php';

class MessagePackRPC_Server
{
  public $port = null;
  public $back = null;
  public $hand = null;
  protected $_listen_socket = null;

  public function __construct($port, $hand, $back = null)
  {
    $this->back = $back == null ? new MessagePackRPC_Back() : $back;
    $this->port = $port;
    $this->hand = $hand;
  }

  public function __destruct()
  {
    $this->close_coket();
  }

  public function close_coket()
  {
    if (is_resource($this->_listen_socket))
      socket_close($this->_listen_socket);
  }

  public function recv()
  {
    try {
      $this->_listen_socket = socket_create_listen($this->port);
      $sockList = array($this->_listen_socket);

      if ($this->_listen_socket === FALSE) {
        throw new Exception(); // TODO:
      }

      // TODO : Server connection check
      // TODO : Server connection outer
      while (TRUE) {
        $moveList = $sockList;
        $moveNums = socket_select($moveList, $w = null, $e = null, null);
        foreach ($moveList as $moveItem) {
          if ($moveItem == $this->_listen_socket) {
            $acptItem   = socket_accept($this->_listen_socket);
            $sockList[] = $acptItem;
          } else {
            $data = socket_read($moveItem, $this->back->size);

            list($code, $func, $args) = $this->back->serverRecvObject($data);
            $hand = $this->hand;
	    $error = null;
	    try {
	      $ret = call_user_func_array(array($hand, $func), $args);
	    } catch (Exception $e) {
	      $ret = null;
	      $error = $e->__toString();
	    }
            $send = $this->back->serverSendObject($code, $ret, $error);
            socket_write($moveItem, $send);

            unset($sockList[array_search($moveItem, $sockList)]);
            socket_close($moveItem);
          }
        }
      }

    } catch (Exception $e) {
      // TODO:
    }
  }
}
