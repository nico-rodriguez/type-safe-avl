#!/usr/bin/env bash

BENCHMARK_VALUES=(10 20 30 40 50 60)

function compile_benchmarks() {
  compile_deps "extern"
  compile_deps "fullextern"
  compile_deps "intern"
  compile_function "Insert"
  compile_function "Delete"
  compile_function "Lookup"
  compile_benchmark "Extern"
  compile_benchmark "FullExtern"
  compile_benchmark "Intern"
}

function compile_deps() {
  APROACH=$1
  echo "Compilando dependencias para $APROACH"
  ( time make benchmarking_${APROACH}_deps ) |& grep 'real' | sed 's/real\t/Real time: /'
}

function compile_function() {
  FUNCTION_NAME=$1
  echo "Compilando benchmarks para $FUNCTION_NAME"
  mkdir -p Benchmarks/Extern
  compile_aproach "extern" $FUNCTION_NAME
  mkdir -p Benchmarks/FullExtern
  compile_aproach "fullextern" $FUNCTION_NAME
  mkdir -p Benchmarks/Intern
  compile_aproach "intern" $FUNCTION_NAME
}

function compile_aproach() {
  APROACH=${1^}
  APROACH=`echo $APROACH | sed 's/extern/Extern/'`
  FUNCTION_NAME=$2
  FILE_RESULTS=Benchmarks/$APROACH/${FUNCTION_NAME}.txt
  touch $FILE_RESULTS
  echo "Compilando $FUNCTION_NAME para $APROACH"
  echo "Tiempos de compilación" >> $FILE_RESULTS
  for i in "${BENCHMARK_VALUES[@]}"; do
    echo "N = $i" >> $FILE_RESULTS
    echo "${APROACH}/${FUNCTION_NAME}/${FUNCTION_NAME}$i"
    ( time make ${APROACH}/${FUNCTION_NAME}/${FUNCTION_NAME}$i ) |& grep 'real' | sed 's/real\t/Real time: /' >> $FILE_RESULTS
  done
}

function compile_benchmark() {
  APROACH=$1
  echo "Compilando benchmark para $APROACH"
  make ${APROACH}/Benchmark
}

function run_benchmarks() {
  run_function "Insert"
  run_function "Delete"
  run_function "Lookup"
  run_benchmark "Extern"
  run_benchmark "FullExtern"
  run_benchmark "Intern"
}

function run_function() {
  FUNCTION_NAME=$1
  echo "Ejecutando benchmarks para $FUNCTION_NAME"
  mkdir -p Benchmarks/Extern
  run_aproach "extern" $FUNCTION_NAME
  mkdir -p Benchmarks/FullExtern
  run_aproach "fullextern" $FUNCTION_NAME
  mkdir -p Benchmarks/Intern
  run_aproach "intern" $FUNCTION_NAME
}

function run_aproach() {
  APROACH=${1^}
  APROACH=`echo $APROACH | sed 's/extern/Extern/'`
  FUNCTION_NAME=$2
  FILE_RESULTS=Benchmarks/$APROACH/${FUNCTION_NAME}.txt
  touch $FILE_RESULTS
  echo "Ejecutando $FUNCTION_NAME para $APROACH"
  echo "Tiempos de ejecución" >> $FILE_RESULTS
  for i in "${BENCHMARK_VALUES[@]}"; do
    echo "N = $i" >> $FILE_RESULTS
    echo "./output_dir/Benchmarking/${APROACH}/${FUNCTION_NAME}/${FUNCTION_NAME}$i"
    ./output_dir/Benchmarking/${APROACH}/${FUNCTION_NAME}/${FUNCTION_NAME}$i
    ( time ./output_dir/Benchmarking/${APROACH}/${FUNCTION_NAME}/${FUNCTION_NAME}$i ) |& grep 'real' | sed 's/real\t/Real time: /' >> $FILE_RESULTS
  done
}

function run_benchmark() {
  APROACH=$1
  FILE_RESULTS=Benchmarks/${APROACH}.txt
  echo "Ejecutando benchmark para $APROACH"
  echo "Tiempos de ejecución" >> $FILE_RESULTS
  ( time ./output_dir/Benchmarking/${APROACH}/Benchmark ) |& grep 'real' | sed 's/real\t/Real time: /' >> $FILE_RESULTS
}

function main() {
  compile_benchmarks
  run_benchmarks
}

main
