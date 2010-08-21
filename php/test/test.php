<?php
include_once dirname(__FILE__) . '/../lib/Back.php';
include_once dirname(__FILE__) . '/../lib/Future.php';
include_once dirname(__FILE__) . '/../lib/Client.php';
include_once dirname(__FILE__) . '/../lib/Server.php';

class Example
{
  public function __construct()
  {
  }

  public function exampleRequest($query)
  {
    try {
      $server = new MessagePackRPC_Server();
      return $server->recv($query);
    } catch (Example $e) {
      echo $e->getMessage() . "\n";
      exit;
    }
  }

  public function example01($val)
  {
    return $val + $val;
  }

  public function example02($val)
  {
    return $val * $val;
  }
}

function testIs($no, $a, $b)
{
  if ($a === $b) {
    echo "OK:{$no}/{$a}/{$b}\n";
  } else {
    echo "NO:{$no}/{$a}/{$b}\n";
  }
}

$object = new Example();
$server = new MessagePackRPC_Server(new MessagePackRPC_Back(), $object);

$back01 = new MessagePackRPC_Back();
$back02 = new MessagePackRPC_Back();

$call01 = $back01->clientCallObject(1, 'example01', array(1, 1));
$recv01 = $server->recv($back01->msgpackEncode($call01));
$f01    = $back01->clientRecvObject($recv01);
testIs('001', 2, $f01->getResult());

$call02 = $back02->clientCallObject(1, 'example02', array(1, 1));
$recv02 = $server->recv($back02->msgpackEncode($call02));
$f02    = $back02->clientRecvObject($recv02);
testIs('002', 1, $f02->getResult());
exit;