<?php
require_once dirname(__FILE__) . '/Back.php';

class MessagePackRPC_Server
{
  public $addr = 0;
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
      $socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
      if (!($socket
       	&& socket_set_option($socket, SOL_SOCKET, SO_REUSEADDR, 1)
	&& socket_bind($socket, $this->addr, $this->port)
	&& socket_listen($socket))) {
        throw new MessagePackRPC_Error_NetworkError(error_get_last());
      }
      $this->_listen_socket = $socket;
      $sockList = array($this->_listen_socket);

      // TODO : Server connection check
      // TODO : Server connection outer
      while (TRUE) {
        $moveList = $sockList;
        $w = null;
        $e = null;
        $moveNums = socket_select($moveList, $w, $e, null);
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
