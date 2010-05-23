<?php
class MessagePackRPC_TCPSocket
{
  public $addr = null;
  public $loop = null;
  public $tprt = null;  // Transport
  public $cltf = null;  // Packet send object

  public function __construct($addr, $loop, $tprt)
  {
    $this->messagePack = new MessagePack();
    $this->messagePack->initialize();

    $this->addr = $addr;
    $this->loop = $loop;
    $this->tprt = $tprt;
  }

  public function tryConnOpening()
  {
    // TODO: Event Loop Implementation
    if ($this->cltf != null) throw new Exception("already connected");
    $host       = $this->addr->getHost();
    $port       = $this->addr->getPort();
    $errs       = "";
    $errn       = "";
    $this->cltf = fsockopen($host, $port, $errn, $errs);

    if ($this->cltf != false) {
      $this->cbConnectedFlg();
    } else {
      $this->cbFailed();
    }
  }

  public function tryMsgPackSend($sendmg = null, $sizerp = 1024)
  {
    // TODO: Event Loop Implementation
    // TODO: Socket Stream
    $buf = null;
    $flg = fputs($this->cltf, $sendmg);
    $buf = fread($this->cltf, $sizerp);
    $this->tryConnClosing();

    $buf = msgpack_unpack($buf);
    $this->cbMsgsReceived($buf);
  }

  public function tryConnClosing()
  {
    // TODO: Event Loop Implementation
    if ((!$this->cltf) && fclose($this->cltf)) {
      $this->cltf = null;
    }
  }

  public function cbConnectedFlg()
  {
    $this->tprt->cbConnectedFlg();
  }

  public function cbConnectFaile($reason = null)
  {
    $this->trySocketClose();
    $this->tprt->cbConnectFaile($reason);
  }

  public function cbMsgsReceived($buffer = null)
  {
    // TODO: Socket Stream
    $this->tprt->cbMsgsReceived($buffer);
  }

  public function cbClosed($reason = null)
  {
    $this->tryConnClosing();
    $this->tprt->cbClosed();
  }

  public function cbFailed($reason = null)
  {
    $this->tryConnClosing();
    $this->tprt->cbFailed();
  }
}

// TODO: Event Loop Implementation
// TODO: Event Loop Implementation