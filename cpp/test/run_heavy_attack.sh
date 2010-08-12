#!/bin/sh
export HEAVY_TEST=1

log_out="log-`date +"%Y-%m-%d_%k-%M-%S"`.log"

cd test 2>/dev/null

export TEST_PROTO=tcp
echo "* tcp test" | tee -a "$log_out"
THREAD=500 LOOP=10 ./attack_connect  2>&1 | tee -a "$log_out"
THREAD=100 LOOP=4  ./attack_huge     2>&1 | tee -a "$log_out"
THREAD=500 LOOP=10 ./attack_pipeline 2>&1 | tee -a "$log_out"

export TEST_PROTO=unix
echo "* unix test" | tee -a "$log_out"
THREAD=500 LOOP=10 ./attack_connect  2>&1 | tee -a "$log_out"
THREAD=100 LOOP=4  ./attack_huge     2>&1 | tee -a "$log_out"
THREAD=500 LOOP=10 ./attack_pipeline 2>&1 | tee -a "$log_out"

export TEST_PROTO=udp
export SIZE=30000
echo "* udp test" | tee -a "$log_out"
THREAD=500 LOOP=10 ./attack_connect  2>&1 | tee -a "$log_out"
THREAD=100 LOOP=4  ./attack_huge     2>&1 | tee -a "$log_out"
THREAD=500 LOOP=10 ./attack_pipeline 2>&1 | tee -a "$log_out"

