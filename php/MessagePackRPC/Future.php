<?php
class MessagePackRPC_Future
{
  public $result = null;
  public $errors = null;
  public $loop   = null;

  public function __construct($loop)
  {
    $this->loop = $loop;
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

  public function join()
  {
    // TODO: Event Loop Implementation

    // while (($this->errors == null) && ($this->result == null)) {
    //   $this->loop->run();
    // }
  }
}