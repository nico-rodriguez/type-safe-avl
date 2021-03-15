#!/usr/bin/env python3

from sys import argv
from subprocess import run
from re import compile, findall, DOTALL
from itertools import product
from multiprocessing import Pool, cpu_count


def valid_bench_names():
    """
    Return a list with all the possible benchmark names in lower case.
    """
    return map(lambda t: t[0] + t[1],
               product(['bst-', 'avl-'], ['unsafe', 'fullextern', 'extern', 'intern']))


def is_valid_bench_name(bench_name):
    """
    Returns True if and only if the given benchmark name is a valid benchmark name.
    """
    valid_names = valid_bench_names()

    return bench_name.lower() in valid_names


USAGE_MESSAGE = """
  Usage: python3 avg-bench.py [BENCH NAME] [N] [SAVE TO FILE] [DEBUG]
  where:
  
  * BENCH NAME (case insensitive) is necessary and must be one of:
  - bst-unsafe, avl-unsafe
  - bst-fullextern, avl-fullextern
  - bst-extern, avl-extern
  - bst-intern, avl-intern,
  - all.
  * N (optional) is a positive integer number which indicates
  how many times to repeat the benchmark (the results
  are averaged). It defaults to 5.
  * SAVE TO FILE (optional) is either True or False (case insensitive)
  and controls whether the results are saved to a file.
  * DEBUG (optional) is either True or False (case insensitive)
  and enables debug printing. Defaults to False.
  """


def exit_with_usage_msg():
    """
    Print the usage message and exit the interpreter.
    """
    print(USAGE_MESSAGE)
    exit()


def sanitize_arguments():
    """
    Sanitize the command line arguments.
    Ignore any extra arguments provided.
    """
    if (len(argv) < 2):
        exit_with_usage_msg()

    elif (len(argv) >= 2):
        bench_name = argv[1].strip().lower()
        if ((not is_valid_bench_name(bench_name)) and bench_name != "all"):
            exit_with_usage_msg()

        if (len(argv) > 2):
            n = argv[2].strip()
            if (not n.isdigit()):
                exit_with_usage_msg()
            else:
                n = int(n)

            if (len(argv) > 3):
                save_to_file = argv[3].strip().lower()
                if (save_to_file == "true"):
                    save_to_file = True
                elif (save_to_file == "false"):
                    save_to_file = False
                else:
                    exit_with_usage_msg()

            if (len(argv) > 4):
                debug = argv[4].strip().lower()
                if (debug == "true"):
                    debug = True
                elif (debug == "false"):
                    debug = False
                else:
                    exit_with_usage_msg()

        else:
            n = 5

    return bench_name, n, save_to_file, debug


def get_running_times(result, debug):
    """
    Parse the text results from the benchmark in order to extract the running times.
    Return a dictionary with keys 'INSERT', 'DELETE' and 'LOOKUP', and arrays as values.
    """
    if (debug):
        print("***get_running_times***", result, sep="\n")
    get_op_times_re = [
        compile(r"INSERT\n(.*)\nDELETE", DOTALL),
        compile(r"DELETE\n(.*)\nLOOKUP", DOTALL),
        compile(r"LOOKUP\n(.*)", DOTALL)
    ]
    get_times = compile(r"N=[\w|^]{1,4}: (\d*\.\d*)s")
    op_names = ["INSERT", "DELETE", "LOOKUP"]
    times = {}

    for i in range(3):
        op_times = get_op_times_re[i].findall(result)[0]
        op_times = get_times.findall(op_times)
        op_times = list(map(float, op_times))
        if (debug):
            print(op_times, sep="\n")
        times[op_names[i]] = op_times
    return times


def get_average_times(times):
    """
    Recives a list of dictionaries from get_running_times and computes the average
    for each function position wise.
    """
    return {
        'INSERT': ['{:.3e}'.format(float(sum(y) / len(times))) for y in zip(*[x['INSERT'] for x in times])],
        'DELETE': ['{:.3e}'.format(float(sum(y) / len(times))) for y in zip(*[x['DELETE'] for x in times])],
        'LOOKUP': ['{:.3e}'.format(float(sum(y) / len(times))) for y in zip(*[x['LOOKUP'] for x in times])]
    }


def run_benchmark(bench_name, bench_num, debug):
    """
    It executes the named benchmark using an exclusive builddir
    (the default is dist/)
    """
    result = run(f"cabal bench {bench_name.lower()} --builddir dist{bench_num}",
                 shell=True, capture_output=True, text=True)
    if (debug):
        print("***run_benchmark***", result, sep="\n")
    return get_running_times(result.stdout, debug)


def execute_benchmarks(bench_name, n, save_to_file, debug):
    """
    This function repeatedly executes (in parallel) the named benchmark
    and returns the average running times.


    @param bench_name: the name of the benchmark to execute. Possible choices are
    [bst|avl]-[unsafe|fullextern|extern|intern]. For instance, 'bst-extern'.

    @param n: the amount of times the benchmark is executed.

    @param save_to_file: boolean which tells whether to save results to a file.

    @param debug: boolean which tells if debug printing is needed.

    @returns: the running times as a dictionary with three entries.
    Each entry has the running times of a different operation.
    The keys are the names of each operation: 'INSERT', 'DELETE', 'LOOKUP'.
    It also saves the results to a file.
    """
    with Pool(min(cpu_count(), n)) as p:
        results = p.starmap(
            run_benchmark, [(bench_name, str(i), debug) for i in range(n)], n)
        results = get_average_times(results)
        if (debug):
            print("***execute_benchmarks***", results, sep="\n")
        if (save_to_file):
            save_results_to_file(f"benchmark/{bench_name}.txt", results)
        return results


def execute_all_benchmarks(n, save_to_file, debug):
    """
    This function repeatedly executes (in parallel) all the benchmarks
    and returns the average running times.

    @param n: the amount of times the benchmark is executed.

    @param save_to_file: boolean which tells whether to save results to a file.

    @param debug: boolean which tells if debug printing is needed.

    @returns: None. It writes the results to different files. The name of
    each file is related to the benchmark name; the contents are those
    from the function execute_benchmarks.
    """
    for bench_name in valid_bench_names():
        results = execute_benchmarks(bench_name, n, save_to_file, debug)
        if (debug):
            print("***execute_all_benchmarks***", results, sep="\n")


def save_results_to_file(file_name, results):
    """
    Save the results of the function execute_benchmarks to a file
    with the following format:
      INSERT
      ...
      DELETE
      ...
      LOOKUP
      ...
    """
    with open(file_name, "w") as f:
        for op in results.keys():
            f.write(op + "\n")
            f.writelines(map(lambda n: str(n) + "\n", results[op]))


if __name__ == '__main__':
    bench_name, n, save_to_file, debug = sanitize_arguments()

    if (bench_name == "all"):
        execute_all_benchmarks(n, save_to_file, debug)
    else:
        results = execute_benchmarks(bench_name, n, save_to_file, debug)
        if (debug):
            print("main", results)
  
