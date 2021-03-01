#!/usr/bin/env python3

from sys import argv
from subprocess import run
from re import compile, findall
from itertools import product
from multiprocessing import Pool


def valid_bench_names():
  """
  Return a list with all the possible benchmark names in lower case.
  """
  return map(lambda t : t[0] + t[1],
    product(['bst-', 'avl-'], ['unsafe', 'fullextern', 'extern', 'intern']))


def is_valid_bench_name(bench_name):
  """
  Returns True if and only if the given benchmark name is a valid benchmark name.
  """
  valid_bench_names = valid_bench_names()

  return bench_name.lower() in valid_bench_names


def _mock_bench1():
  return """Benchmark avl-fullextern: RUNNING...
INSERT
N=10: 0.000003685s
N=20: 0.000020093s
N=30: 0.000018883s
N=40: 0.00001599s
N=50: 0.000019083s
N=60: 0.000032133s
N=70: 0.000019902s
N=80: 0.000016839s
N=90: 0.000017733s
N=100: 0.000011346s
DELETE
N=10: 0.000010664s
N=20: 0.000010929s
N=30: 0.000010111s
N=40: 0.000008648s
N=50: 0.000008447s
N=60: 0.000008667s
N=70: 0.000008291s
N=80: 0.000012023s
N=90: 0.000008058s
N=100: 0.000008055s
LOOKUP
N=10: 0.000052813s
N=20: 0.000129225s
N=30: 0.0002612s
N=40: 0.000306716s
N=50: 0.00041834s
N=60: 0.000279074s
N=70: 0.000297553s
N=80: 0.000370009s
N=90: 0.000598961s
N=100: 0.000394298s
Benchmark avl-fullextern: FINISH
"""

def _mock_bench2():
  return """Benchmark avl-unsafe: RUNNING...
INSERT
N=2^6: 0.000389468s
N=2^7: 0.001001006s
N=2^8: 0.002546033s
N=2^9: 0.00836335s
N=2^10: 0.03130746s
N=2^11: 0.122868723s
N=2^12: 0.483508947s
N=2^13: 1.916203996s
N=2^14: 7.785558802s
N=2^15: 32.371207625s
DELETE
N=2^6: 0.000293852s
N=2^7: 0.001020235s
N=2^8: 0.004021208s
N=2^9: 0.015454593s
N=2^10: 0.060563177s
N=2^11: 0.24069094s
N=2^12: 0.949627327s
N=2^13: 3.788817933s
N=2^14: 15.341255012s
N=2^15: 62.965286092s
LOOKUP
N=2^6: 0.000007022s
N=2^7: 0.000004781s
N=2^8: 0.000004236s
N=2^9: 0.000005014s
N=2^10: 0.000004838s
N=2^11: 0.00000476s
N=2^12: 0.000004596s
N=2^13: 0.000004514s
N=2^14: 0.000004545s
N=2^15: 0.000003434s
Benchmark avl-unsafe: FINISH
"""

def get_running_times(result):
  """
  Parse the text results from the benchmark in order to extract the running times.
  Return a dictionary with keys 'INSERT', 'DELETE' and 'LOOKUP', and arrays as values.
  """
  get_times_re = compile('N=[\w|^]{1,4}: (\d*\.\d*)s')
  times = get_times_re.findall(result)
  times = list(map(float, times))
  return {
    'INSERT': times[0:10],
    'DELETE': times[10:20],
    'LOOKUP': times[20:30]
  }

def get_average_times(times):
  """
  Recives a list of dictionaries from get_running_times and computes the average
  for each function position wise.
  """
  return {
      'INSERT': [float('{:0.3e}'.format(sum(y) / len(times))) for y in zip(*[x['INSERT'] for x in times])],
      'DELETE': [float('{:0.3e}'.format(sum(y) / len(times))) for y in zip(*[x['DELETE'] for x in times])],
      'LOOKUP': [float('{:0.3e}'.format(sum(y) / len(times))) for y in zip(*[x['LOOKUP'] for x in times])]
  }

def run_benchmark(bench_name):
  result = run(['cabal', "bench", bench_name.lower()], capture_output=True, text=True)
  return get_running_times(result.stdout)

def execute_benchmarks(bench_name, n):
  """
  This function repeatedly executes (in parallel) the named benchmark
  and returns the average running times.

  @param bench_name: the name of the benchmark to execute. Possible choices are
  [bst|avl]-[unsafe|fullextern|extern|intern]. For instance, 'bst-extern'.
  @param n: the amount of times the benchmark is executed.
  @returns: the running times as a dictionary with three entries. Each entry has
  the running times of a different operation. The keys are the names of each
  operation: 'INSERT', 'DELETE', 'LOOKUP'.
  """
  with Pool(n) as p:
    results = p.map(run_benchmark, [bench_name for _ in range(n)])
    return get_average_times(results)



if __name__ == '__main__':
  bench_name = argv[1]
  if (len(argv) > 2):
    n = int(argv[2])
  else:
    n = 5

  print(execute_benchmarks(bench_name, n))
