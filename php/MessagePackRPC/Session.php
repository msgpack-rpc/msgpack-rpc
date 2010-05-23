<?php
require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . "TCPTransport.php");
require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . "Future.php");

class MessagePackRPC_Session
{
  public $addr = null;
  public $loop = null;
  public $rtbl = null; // Request Table
  public $tprt = null; // Transport
  public $msgc =    1;

  public function __construct($addr, $loop)
  {
    $this->rtbl = array();
    $this->addr = $addr;
    $this->loop = $loop;
    $this->tprt = null;
  }

  public function sendingRequest($mt, $a)
  {
    $i = $this->generatesMsgId();
    $l = $this->loop;
    $f = new MessagePackRPC_Future($l);

    $this->rtbl[$i]  =& $f;
    $this->tprt      = $this->getCreatesTprt();
    $this->tprt->tryBufDataSend(array(0, intval($i), strval($mt), $a));
    return $f;
  }

  public function gettomgAddress()
  {
    return $this->addr;
  }

  public function tryConnClosing()
  {
    if ($this->tprt != null) $this->tprt->tryConnClosing();
    $this->tprt = null;
  }

  public function generatesMsgId()
  {
    $i = $this->msgc;
    $this->msgc += 1;

    if ($this->msgc > (1 << 30))
      $this->msgc = 0;

    return $i;
  }

  public function getCreatesTprt()
  {
    if ($this->tprt != null) return $this->tprt;
    $tt =  new MessagePackRPC_TCPTransport($this, $this->loop);
    $this->tprt = $tt;
    return $tt;
  }

  public function cbConnectFaile($reason)
  {
    foreach ($this->rtbl as $i => $fut) {
      $fut->setErrors($reason);
    }
    $this->rtbl   = array();
    $this->tryConnClosing();
    $this->loop->end();
  }

  public function cbMsgsReceived($buffer)
  {
    if (count($buffer) != 4) {
      throw new Exception("invalid mprpc protocol");
    }

    $msgtype = $buffer[0];
    $msgid   = $buffer[1];
    $msgerr  = $buffer[2];
    $msgret  = $buffer[3];

    if ($msgtype      != 1) {
      throw new Exception("invalid mprpc protocol");
    }

    if (!isset($this->rtbl[$msgid])) {
      throw new Exception("unknown msgid");
    }

    $fut =& $this->rtbl[$msgid];
    // unset($this->rtbl[$msgid]);

    if (!empty($msgerr)) {
      $fut->setErrors($msgerr);
    } else {
      $fut->setResult($msgret);
    }

    $this->loop->end();
  }

  public function cbClosed()
  {
    foreach ($this->rtbl as $i => $fut) {
      $fut->setErrors($reason);
    }
    $this->rtbl   = array();
    $this->tryConnClosing();
    $this->loop->end();
  }

  public function cbFailed()
  {
    foreach ($this->rtbl as $i => $fut) {
      $fut->setErrors("faile");
    }
    $this->rtbl   = array();
    $this->tryConnClosing();
    $this->loop->end();
  }
}
