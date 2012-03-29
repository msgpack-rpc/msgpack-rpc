<?php
// Please run client.php
include_once dirname(__FILE__) . '/../lib/Back.php';
include_once dirname(__FILE__) . '/../lib/Future.php';
include_once dirname(__FILE__) . '/../lib/Client.php';
include_once dirname(__FILE__) . '/../lib/Server.php';

class App
{
  public function hello1($a)
  {
    return $a + 1;
  }

  public function hello2($a)
  {
    return $a + 2;
  }

  public function fail()
  {
    throw new Exception('hoge');
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

try {
  $server = new MessagePackRPC_Server('1985', new App());
  $server->recv();
} catch (Exception $e) {
  echo $e->getMessage() . "\n";
}
exit;
