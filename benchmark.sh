#!/usr/bin/env bash

BENCHMARK_VALUES=(10 200 300 400 500 600)

function compile_benchmarks() {
  compile_deps "extern"
  compile_deps "fullextern"
  compile_deps "intern"
  compile_function "Insert"
  compile_function "Delete"
  compile_function "Lookup"
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
  FILE_RESULTS=Benchmarks/$APROACH/$FUNCTION_NAME
  touch $FILE_RESULTS
  echo "Compilando $FUNCTION_NAME para $APROACH"
  echo "Tiempos de compilación" >> $FILE_RESULTS
  for i in "${BENCHMARK_VALUES[@]}"; do
    echo "N = $i" >> $FILE_RESULTS
    ( time make ${APROACH}/${FUNCTION_NAME}/${FUNCTION_NAME}$i ) |& grep 'real' | sed 's/real\t/Real time: /' >> $FILE_RESULTS
  done
}

function run_benchmarks() {
  run_function "Insert"
  run_function "Delete"
  run_function "Lookup"
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
  FILE_RESULTS=Benchmarks/$APROACH/$FUNCTION_NAME
  touch $FILE_RESULTS
  echo "Ejecutando $FUNCTION_NAME para $APROACH"
  echo "Tiempos de ejecución" >> $FILE_RESULTS
  for i in "${BENCHMARK_VALUES[@]}"; do
    echo "N = $i" >> $FILE_RESULTS
    ( time ./Benchmarks/${APROACH}/${FUNCTION_NAME}/${FUNCTION_NAME}$i ) |& grep 'real' | sed 's/real\t/Real time: /' >> $FILE_RESULTS
  done
}

function main() {
  compile_benchmarks
  run_benchmarks
}

main
