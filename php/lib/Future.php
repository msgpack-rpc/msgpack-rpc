<?php
require_once dirname(__FILE__) . '/Error.php';

class MessagePackRPC_Future
{
  public $result = null;
  public $errors = null;

  public function __construct()
  {
  }

  public function setResult($result)
  {
    $this->result = $result;
  }

  public function getResult()
  {
    $result = $this->result;
    return $result;
  }

  public function setErrors($errors)
  {
    $this->errors = $errors;
  }

  public function getErrors()
  {
    $errors = $this->errors;
    return $errors;
  }
}
