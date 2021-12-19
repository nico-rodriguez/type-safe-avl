#!/usr/bin/env python3

from sys import argv
from subprocess import run
from re import compile, findall, DOTALL
from itertools import product
from multiprocessing import Pool, cpu_count
from math import sqrt


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
  Usage: python3 avg-bench.py [BENCH NAME] [BENCH TYPE] [N] [SAVE TO FILE] [DEBUG]
  where:
  
  * BENCH NAME (case insensitive) is necessary and must be one of:
  - bst-unsafe, avl-unsafe
  - bst-fullextern, avl-fullextern
  - bst-extern, avl-extern
  - bst-intern, avl-intern.
  * BENCH TYPE (case insensitive) is the type of benchmark to be executed
  (either for running time or compilation time):
  - run
  - compile
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
    save_to_file, debug = False, False
    if (len(argv) < 3):
        exit_with_usage_msg()

    elif (len(argv) >= 3):
        bench_name = argv[1].strip().lower()
        bench_type = argv[2].strip().lower()
        if (not is_valid_bench_name(bench_name)):
            exit_with_usage_msg()
        if ((not bench_type in ["run", "compile"])):
            exit_with_usage_msg()

        if (len(argv) > 3):
            n = argv[3].strip()
            if (not n.isdigit()):
                exit_with_usage_msg()
            else:
                n = int(n)

            if (len(argv) > 4):
                save_to_file = argv[4].strip().lower()
                if (save_to_file == "true"):
                    save_to_file = True
                elif (save_to_file == "false"):
                    save_to_file = False
                else:
                    exit_with_usage_msg()

            if (len(argv) > 5):
                debug = argv[5].strip().lower()
                if (debug == "true"):
                    debug = True
                elif (debug == "false"):
                    debug = False
                else:
                    exit_with_usage_msg()

        else:
            n = 5

    return bench_name, bench_type, n, save_to_file, debug


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


def get_average_running_times(times):
    """
    Recives a list of dictionaries from get_running_times and computes the average
    for each function position wise.
    """
    return {
        'INSERT': [('{:.4e}'.format(float(sum(y) / len(times))), '{:.4e}'.format(std_dev_spread(y))) for y in zip(*[x['INSERT'] for x in times])],
        'DELETE': [('{:.4e}'.format(float(sum(y) / len(times))), '{:.4e}'.format(std_dev_spread(y))) for y in zip(*[x['DELETE'] for x in times])],
        'LOOKUP': [('{:.4e}'.format(float(sum(y) / len(times))), '{:.4e}'.format(std_dev_spread(y))) for y in zip(*[x['LOOKUP'] for x in times])]
    }


def run_time_benchmark(bench_name, bench_num, debug):
    """
    It executes the named benchmark using an exclusive builddir
    (the default is dist/)
    """
    result = run(f"cabal bench {bench_name.lower()} --builddir dist{str(bench_num)}",
                 shell=True, capture_output=True, text=True)
    if (debug):
        print("***run_time_benchmark***", result, sep="\n")
    return get_running_times(result.stdout, debug)


def compile_examples(bench_id, debug):
    """
    Compile the examples modules which contain the tree that are the basis
    for the insert, delete and lookup benchmarks.
    """
    result = run(f'cabal build {bench_name}-example{bench_id} --builddir dist',
                 shell=True, capture_output=True, text=True)
    if (debug):
          print("***compile_examples***", result, sep="\n")
    return 0


def compilation_time_benchmark(bench_name, operation, bench_id, debug):
    """
    It executes the named benchmark using an exclusive builddir
    (the default is dist/) and returns the number it took (in seconds).
    """
    result = run(f'(/usr/bin/time -f "%e" cabal build {bench_name}-{operation}{bench_id} --builddir dist) 2>&1 > /dev/null | tail -1',
                 shell=True, capture_output=True, text=True)
    if (debug):
        print("***compilation_time_benchmark***", result, sep="\n")
    return (operation, float(result.stdout))


def execute_run_time_benchmarks(bench_name, n, save_to_file, debug):
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
            run_time_benchmark, [(bench_name, i, debug) for i in range(n)], n)
        results = get_average_running_times(results)
        if (debug):
            print("***execute_run_time_benchmarks***", results, sep="\n")
        if (save_to_file):
            save_results_to_file(
                f"benchmark/{bench_name}-run-times.txt", results)
        remove_dist_folders()
        return results


def remove_dist_folders():
    """
    Remove the dist folders generated by the benchmarks.
    """
    run("rm -fr dist*", shell=True)
    return 0


def execute_compilation_time_benchmarks(bench_name, n, save_to_file, debug):
    """
    This function repeatedly executes (in parallel) the named benchmark
    and returns the average compilation times.


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
    bench_ops = ["insert", "delete", "lookup"]
    if ("unsafe" in bench_name):
        bench_ids = [str(i) for i in range(1, 11, 1)]
    elif ("fullextern" in bench_name):
        bench_ids = [str(i) for i in range(10, 110, 10)]
    else:   # ("extern" in bench_name) or ("intern" in bench_name)
        if ("avl" in bench_name):
            bench_ids = [str(i) for i in range(10, 70, 10)]
        else:
            bench_ids = [str(i) for i in range(10, 60, 10)]

    times = {}
    for op in bench_ops:
        times[op] = []

    for bench_id in bench_ids:
        unprocessed_times = {
            "insert": [],
            "delete": [],
            "lookup": []
        }
        for _ in range(n):
            compile_examples(bench_id, debug)
            with Pool(min(cpu_count(), len(bench_ops))) as p:
                bench_times = p.starmap(
                    compilation_time_benchmark, [(bench_name, op, bench_id, debug) for op in bench_ops], len(bench_ops))
                for op, t in bench_times:
                    unprocessed_times[op].append(t)
            remove_dist_folders()
        for op in bench_ops:
            unprocessed_times[op] = remove_outliers(unprocessed_times[op])
            avg_time = float('{:.4f}'.format(
                sum(unprocessed_times[op]) / len(unprocessed_times[op])))
            std_dev_time = std_dev(unprocessed_times[op])
            times[op].append((avg_time, std_dev_time))

    if (debug):
        print("***execute_compilation_time_benchmarks***", times, sep="\n")
    if (save_to_file):
        save_results_to_file(
            f"benchmark/{bench_name}-compilation-times.txt", times)
    return times


def save_results_to_file(file_name, results):
    """
    Save the results of the function execute_run_time_benchmarks to a file
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


def mean(arr):
    """
    Compute the mean value of an array of numbers.
    """
    s = sum(arr)
    return s / len(arr)


def std_dev(arr):
    """
    Compute the standard deviation of an array of numbers.
    """
    m = mean(arr)
    s = 0
    for a in arr:
        s += (a - m) ** 2
    return sqrt(s / len(arr))


def std_dev_spread(*vals):
    """
    Compute the standard deviation over a variable list of number arguments.
    """
    m = sum(vals) / len(vals)
    s = 0
    for a in vals:
        s += (a - m) ** 2
    return sqrt(s / len(vals))


def median(arr):
    """
    Compute the median value of an array of numbers.
    """
    arr_copy = arr.copy()
    arr_copy.sort()
    if (len(arr_copy) % 2 == 0):
        i2 = len(arr_copy) // 2
        q2 = (arr_copy[i2] + arr_copy[i2 - 1]) / 2
    else:
        i2 = len(arr_copy) // 2
        q2 = arr_copy[i2]
    return q2


def split_array(arr):
    """
    Split and array in two halves, along its median value.
    It doesn't modify the original array.
    """
    arr_copy = arr.copy()
    arr_copy.sort()
    i2 = len(arr_copy) // 2
    first_half = arr_copy[0:i2]
    if (len(arr_copy) % 2 == 0):
        second_half = arr_copy[i2:]
    else:
        second_half = arr_copy[i2 + 1:]
    return first_half, second_half


def remove_outliers(arr):
    """
    Remove outliers from an array of benchmark times.
    """
    # Compute the first (q1), second (q2) and third (q3) quartile.
    first_half, second_half = split_array(arr)
    q1, q2, q3 = median(first_half), median(arr), median(second_half)
    # Remove values outside 1.5 times the IQR centered at the median
    iqr = q3 - q1
    filtered_arr = list(filter(lambda n: n >= q2 - 1.5 * iqr, arr))
    filtered_arr = list(filter(lambda n: n <= q2 + 1.5 * iqr, filtered_arr))
    return filtered_arr



if __name__ == '__main__':
    bench_name, bench_type, n, save_to_file, debug = sanitize_arguments()

    if (bench_type == "run"):
        results = execute_run_time_benchmarks(
            bench_name, n, save_to_file, debug)
    else:   # bench_type == "compile"
        results = execute_compilation_time_benchmarks(
            bench_name, n, save_to_file, debug)
    if (debug):
        print("main", results)
