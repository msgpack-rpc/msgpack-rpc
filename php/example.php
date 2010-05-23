<?php
require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . "MsgPackClient.php");
try {
  $c = new MsgPackClient('127.0.0.1', '19850');
  echo $c->call('hello1', array(1)) . "\n";
} catch (Exception $e) {
  echo $e->getMessage(). "\n";
}