<?php
require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . "MsgPackTCPSocket.php");

class MsgPackTCPTransport
{
  public $pendingMsgs = null;
  public $messagePack = null;
  public $sess        = null;
  public $loop        = null;
  public $sock        = null;
  public $iscn        = null; // Connecting Flag
  public $isop        = null; // Connected  Flag

  public function __construct($sess, $loop)
  {
    $this->messagePack = new MessagePack();
    $this->pendingMsgs = array();
    $this->sock        = new MsgPackTCPSocket($sess->addr, $loop, $this);
    $this->sess        = $sess;
    $this->loop        = $loop;
    $this->isop        = false;
    $this->iscn        = false;

    $this->messagePack->initialize();
  }

  public function tryBufDataSend($buffer = null)
  {
    $pm = msgpack_pack($buffer);
    if ($this->iscn) {
      $this->sock->tryMsgPackSend($pm);
    } else {
      $this->pendingMsgs[] = $pm;                      // TODO: Socket Stream
      if (!$this->isop) $this->sock->tryConnOpening(); // TODO: Socket Stream
      if (!$this->isop) $this->sock->isop     = false; // TODO: Where TCPSocket#isop
    }
  }

  public function trySendPending()
  {
    foreach ($this->pendingMsgs as $pm) {
      $this->sock->tryMsgPackSend($pm);
    }
    $this->pendingMsgs = array();
  }

  public function tryConnClosing()
  {
    if ($this->sock != null) $this->sock->tryConnClosing();

    $this->pendingMsgs = array();
    $this->iscn        = false;
    $this->isop        = false;
    $this->sock        =  null;
  }

  public function cbConnectedFlg()
  {
    $this->iscn        =  true;
    $this->isop        = false;
    $this->trySendPending();
  }

  public function cbConnectFaile($reason = null)
  {
    $this->tryConnClosing();
    $this->sess->cbConnectFaile($reason);
  }

  public function cbMsgsReceived($buffer = null)
  {
    // TODO: Socket Stream
    $this->sess->cbMsgsReceived($buffer);
  }

  public function cbClosed($reason = null)
  {
    $this->tryConnClosing();
    $this->sess->cbClosed();
  }

  public function cbFailed($reason = null)
  {
    $this->tryConnClosing();
    $this->sess->cbFailed();
  }
}