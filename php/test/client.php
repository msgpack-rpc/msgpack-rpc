<?php
// Please run server.php
include_once dirname(__FILE__) . '/../lib/Back.php';
include_once dirname(__FILE__) . '/../lib/Future.php';
include_once dirname(__FILE__) . '/../lib/Client.php';
include_once dirname(__FILE__) . '/../lib/Server.php';

function testIs($no, $a, $b)
{
  if ($a === $b) {
    echo "OK:{$no}/{$a}/{$b}\n";
  } else {
    echo "NO:{$no}/{$a}/{$b}\n";
  }
}

try {
  $client = new MessagePackRPC_Client('localhost', '1985');
  testIs('test0001', 3, $client->call('hello1', array(2)));
  testIs('test0001', 5, $client->call('hello2', array(3)));
} catch (Exception $e) {
  echo $e->getMessage() . "\n";
}
exit;