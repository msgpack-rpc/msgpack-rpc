#!/bin/sh
#export HEAVY_TEST=1

log_out="log-`date +"%Y-%m-%d_%k-%M-%S"`.log"

cd test 2>/dev/null

export TEST_PROTO=tcp
./attack_connect  2>&1 | tee -a $log_out
./attack_huge     2>&1 | tee -a $log_out
./attack_pipeline 2>&1 | tee -a $log_out

export TEST_PROTO=unix
./attack_connect  2>&1 | tee -a $log_out
./attack_huge     2>&1 | tee -a $log_out
./attack_pipeline 2>&1 | tee -a $log_out

export TEST_PROTO=udp
export SIZE=30000
./attack_connect  2>&1 | tee -a $log_out
./attack_huge     2>&1 | tee -a $log_out
./attack_pipeline 2>&1 | tee -a $log_out

