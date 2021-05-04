from pandas import DataFrame, Series, concat
from seaborn import scatterplot, lineplot
from matplotlib.pyplot import savefig
from scipy.optimize import curve_fit
from numpy import exp2, log2, arange, array, abs


bst_unsafe_running_times_df = DataFrame({
    "bench_name": Series(["bst_unsafe" for _ in range(10 * 3)], dtype="category"),
    "bench_type": Series(["run" for _ in range(10 * 3)], dtype="category"),
    "N": Series([16 for _ in range(10 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in [pow(2, i) for i in range(6, 16, 1)]], dtype="int32"),
    "time": Series([5.411e-05, 1.349e-04, 1.719e-04, 3.670e-04, 7.287e-04, 1.439e-03, 2.899e-03, 5.621e-03, 1.130e-02, 2.237e-02, 4.384e-05, 7.908e-05, 1.597e-04, 3.492e-04, 6.791e-04, 1.391e-03, 2.801e-03, 5.472e-03, 1.067e-02, 2.319e-02, 9.416e-06, 7.837e-06, 1.318e-05, 2.219e-05, 3.876e-05, 6.348e-05, 1.006e-04, 1.626e-04, 2.918e-04, 5.864e-04], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(10)], dtype="category")
})

avl_unsafe_running_times_df = DataFrame({
    "bench_name": Series(["avl_unsafe" for _ in range(10 * 3)], dtype="category"),
    "bench_type": Series(["run" for _ in range(10 * 3)], dtype="category"),
    "N": Series([16 for _ in range(10 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in [pow(2, i) for i in range(6, 16, 1)]], dtype="int32"),
    "time": Series([5.334e-05, 8.713e-05, 1.959e-04, 3.446e-04, 7.078e-04, 1.958e-03, 8.063e-03, 1.680e-02, 3.167e-02, 6.317e-02, 3.239e-04, 8.361e-05, 1.695e-04, 3.246e-04, 8.450e-04, 4.873e-03, 5.608e-03, 1.675e-02, 3.130e-02, 6.559e-02, 7.167e-06, 3.066e-06, 2.546e-06, 2.689e-06, 2.812e-06, 2.748e-06, 2.852e-06, 2.924e-06, 2.882e-06, 2.741e-06], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(10)], dtype="category")
})

bst_fullextern_compilation_times_df = DataFrame({
    "bench_name": Series(["bst_fullextern" for _ in range(10 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(10 * 3)], dtype="category"),
    "N": Series([16 for _ in range(10 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 110, 10)], dtype="int8"),
    "time": Series([2.25, 2.66, 3.45, 4.43, 5.69, 7.59, 9.82, 12.67, 16.22, 20.09, 2.16, 2.59, 3.37, 4.43, 5.76, 7.66, 9.99, 12.98, 16.7, 20.62, 2.15, 2.58, 3.36, 4.4, 5.77, 7.56, 9.91, 12.8, 16.43, 20.32], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(10)], dtype="category")
})

avl_fullextern_compilation_times_df = DataFrame({
    "bench_name": Series(["avl_fullextern" for _ in range(10 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(10 * 3)], dtype="category"),
    "N": Series([16 for _ in range(10 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 110, 10)], dtype="int8"),
    "time": Series([2.46, 2.86, 3.55, 4.66, 6.13, 7.71, 10.25, 13.82, 18.11, 22.89, 2.3, 2.78, 3.46, 4.67, 6.19, 7.81, 10.51, 14.26, 18.73, 23.33, 2.35, 2.73, 3.39, 4.52, 6.01, 7.47, 10.06, 13.74, 17.96, 22.66], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(10)], dtype="category")
})

bst_extern_compilation_times_df = DataFrame({
    "bench_name": Series(["bst_extern" for _ in range(5 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(5 * 3)], dtype="category"),
    "N": Series([4 for _ in range(5 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 60, 10)], dtype="int8"),
    "time": Series([3.7, 7.69, 18.66, 42.85, 85.76, 2.44, 6.8, 17.98, 41.76, 85.0, 2.92, 6.3, 16.48, 38.91, 79.85], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(5)], dtype="category")
})

avl_extern_compilation_times_df = DataFrame({
    "bench_name": Series(["avl_extern" for _ in range(6 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(6 * 3)], dtype="category"),
    "N": Series([16 for _ in range(6 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 70, 10)], dtype="int8"),
    "time": Series([4.03, 5.09, 7.1, 11.0, 15.45, 19.9, 3.26, 4.48, 6.51, 9.99, 14.39, 19.29, 3.17, 4.4, 6.28, 9.76, 14.05, 18.78], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(6)], dtype="category")
})

bst_intern_compilation_times_df = DataFrame({
    "bench_name": Series(["bst_intern" for _ in range(5 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(5 * 3)], dtype="category"),
    "N": Series([4 for _ in range(5 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 60, 10)], dtype="int8"),
    "time": Series([3.32, 6.9, 17.4, 40.52, 82.39, 2.7, 6.43, 16.82, 39.86, 81.8, 2.68, 5.85, 15.53, 37.54, 77.06], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(5)], dtype="category")
})

avl_intern_compilation_times_df = DataFrame({
    "bench_name": Series(["avl_intern" for _ in range(6 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(6 * 3)], dtype="category"),
    "N": Series([16 for _ in range(6 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 70, 10)], dtype="int8"),
    "time": Series([3.14, 4.12, 5.44, 7.69, 10.57, 13.81, 2.49, 3.48, 4.73, 7.09, 9.95, 13.17, 2.48, 3.42, 4.8, 6.95, 9.82, 13.06], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(6)], dtype="category")
})

all_bench_df = bst_unsafe_running_times_df.append(avl_unsafe_running_times_df, ignore_index=True).append(bst_fullextern_compilation_times_df, ignore_index=True).append(avl_fullextern_compilation_times_df, ignore_index=True).append(
    bst_extern_compilation_times_df, ignore_index=True).append(avl_extern_compilation_times_df, ignore_index=True).append(bst_intern_compilation_times_df, ignore_index=True).append(avl_intern_compilation_times_df, ignore_index=True)
all_bench_df = all_bench_df.astype({"bench_name": "category", "bench_type": "category",
                                   "N": "int8", "size": "int32", "time": "float32", "operation": "category"}, copy=True)


def exp_fun(x, a, b, c):
    return a * exp2(-b * x) + c

def linear_fun(x, a, b):
    return a * x + b

def get_fit_fun_params(data, bench_name, op):
    y_max = data[(data.bench_name == bench_name) & (data.operation == op)]["time"].max()
    x_max = data[(data.bench_name == bench_name) & (data.operation == op) & (data.time == y_max)]["size"].iloc[0]
    y_min = data[(data.bench_name == bench_name) & (data.operation == op)]["time"].min()
    x_min = data[(data.bench_name == bench_name) & (data.operation == op) & (data.time == y_min)]["size"].iloc[0]

    if ("unsafe" in bench_name):
        fun = linear_fun
        fun_legend = f"Ajuste lineal"
        b = (y_min * x_max - y_max * x_min) / (x_max - x_min)
        a = (y_max - b) / x_max
        p0 = (a, b)
    else:
        fun = exp_fun
        fun_legend = f"Ajuste exponencial"
        p0 = (1, - log2(y_max) / x_max, 0)
    
    return fun, fun_legend, p0

def plot_single_operation(data, op, fit):
    """
    Given a DataFrame with times for a single operation,
    plot a scatter plot with its times.
    """
    x_min, x_max = data["size"].min(), data["size"].max()
    x_fit = arange(x_min - 5, x_max + 5, 0.2)

    op_data = data[data.operation == op.upper()][["size", "time"]]

    plt = scatterplot(x="size", y="time", data=op_data)

    tree_type = "AVL" if "avl" in data["bench_name"].iloc[0].lower() else "BST"
    data_legend = f"{tree_type}"

    if (fit):
        bench_name = data["bench_name"].iloc[0]
        fun, fun_legend, p0 = get_fit_fun_params(data, bench_name, op)

        popt, ss, max_s = fit_function(op_data, fun, p0)
        y_fit = list(map(lambda x: fun(x, *popt), x_fit))

        plt = lineplot(x=x_fit, y=y_fit)
        print(bench_name, op.lower(), f"- ss={'{:.2e}'.format(ss)}/max_s={'{:.2e}'.format(max_s)}", sep=" ")

        return plt, ([fun_legend], [data_legend])

    return plt, ([], [data_legend])

def set_plot_labels(plt, title, xlabel, ylabel, legends):
    plt.set_title(title)
    plt.set_xlabel(xlabel)
    plt.set_ylabel(ylabel)
    plt.legend(legends)
    return plt

def save_plot(plt, filename):
    fig = plt.get_figure()
    fig.savefig(filename)
    fig.clf()


def fit_function(data, fun, p0):
    """
    Given a DataFrame with operation times (for a single operation),
    fit a given parametrized function to it.
    Returns the adjusted function parameters and the fit error (sum of squares).
    """
    popt, _ = curve_fit(fun, data["size"], data["time"], method="lm", p0=p0)
    y_i = array(data["time"])
    f_i = array(list(map(lambda x: fun(x, *popt), data["size"])))
    squares = (y_i - f_i) ** 2
    ss = sum(squares)
    max_s = squares.max()
    return popt, ss, max_s


def _combine_legend_lists(*args):
    """
    Combines any number of tuples with two lists of legend labels into
    a single list of legend labels.
    """
    legend_list1, legend_list2 = [], []

    for list1, list2 in args:
        legend_list1.extend(list1)
        legend_list2.extend(list2)
    
    legend_list1.extend(legend_list2)

    return legend_list1



if __name__ == '__main__':
    ##### Unsafe
    ###   BST
    plt, legend_list = plot_single_operation(bst_unsafe_running_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST no seguro: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/unsafe/bst_unsafe_insert.png")
    plt, legend_list = plot_single_operation(bst_unsafe_running_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST no seguro: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/unsafe/bst_unsafe_delete.png")
    plt, legend_list = plot_single_operation(bst_unsafe_running_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "BST no seguro: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/unsafe/bst_unsafe_lookup.png")
    ###   AVL
    plt, legend_list = plot_single_operation(avl_unsafe_running_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "AVL no seguro: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/unsafe/avl_unsafe_insert.png")
    plt, legend_list = plot_single_operation(avl_unsafe_running_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "AVL no seguro: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/unsafe/avl_unsafe_delete.png")
    plt, legend_list = plot_single_operation(avl_unsafe_running_times_df, "LOOKUP", False)
    plt = set_plot_labels(plt, "AVL no seguro: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/unsafe/avl_unsafe_lookup.png")
    ###   BST/AVL per operation
    _, legend_list_bst = plot_single_operation(bst_unsafe_running_times_df, "INSERT", True)
    plt, legend_list_avl = plot_single_operation(avl_unsafe_running_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST/AVL: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/unsafe/bst_vs_avl_unsafe_insert.png")
    _, legend_list_bst = plot_single_operation(bst_unsafe_running_times_df, "DELETE", True)
    plt, legend_list_avl = plot_single_operation(avl_unsafe_running_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST/AVL: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/unsafe/bst_vs_avl_unsafe_delete.png")
    _, legend_list_bst = plot_single_operation(bst_unsafe_running_times_df, "LOOKUP", True)
    plt, legend_list_avl = plot_single_operation(avl_unsafe_running_times_df, "LOOKUP", False)
    plt = set_plot_labels(plt, "BST/AVL: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/unsafe/bst_vs_avl_unsafe_lookup.png")

    ##### Fullextern
    ###   BST
    plt, legend_list = plot_single_operation(bst_fullextern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST completamente externalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/fullextern/bst_fullextern_insert.png")
    plt, legend_list = plot_single_operation(bst_fullextern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST completamente externalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/fullextern/bst_fullextern_delete.png")
    plt, legend_list = plot_single_operation(bst_fullextern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "BST completamente externalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/fullextern/bst_fullextern_lookup.png")
    ###   AVL
    plt, legend_list = plot_single_operation(avl_fullextern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "AVL completamente externalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/fullextern/avl_fullextern_insert.png")
    plt, legend_list = plot_single_operation(avl_fullextern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "AVL completamente externalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/fullextern/avl_fullextern_delete.png")
    plt, legend_list = plot_single_operation(avl_fullextern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "AVL completamente externalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/fullextern/avl_fullextern_lookup.png")
    ###   BST/AVL per operation
    _, legend_list_bst = plot_single_operation(bst_fullextern_compilation_times_df, "INSERT", True)
    plt, legend_list_avl = plot_single_operation(avl_fullextern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST/AVL: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/fullextern/bst_vs_avl_fullextern_insert.png")
    _, legend_list_bst = plot_single_operation(bst_fullextern_compilation_times_df, "DELETE", True)
    plt, legend_list_avl = plot_single_operation(avl_fullextern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST/AVL: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/fullextern/bst_vs_avl_fullextern_delete.png")
    _, legend_list_bst = plot_single_operation(bst_fullextern_compilation_times_df, "LOOKUP", True)
    plt, legend_list_avl = plot_single_operation(avl_fullextern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "BST/AVL: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/fullextern/bst_vs_avl_fullextern_lookup.png")

    ##### Extern
    ###   BST
    plt, legend_list = plot_single_operation(bst_extern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST externalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/extern/bst_extern_insert.png")
    plt, legend_list = plot_single_operation(bst_extern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST externalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/extern/bst_extern_delete.png")
    plt, legend_list = plot_single_operation(bst_extern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "BST externalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/extern/bst_extern_lookup.png")
    ### AVL
    plt, legend_list = plot_single_operation(avl_extern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "AVL externalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/extern/avl_extern_insert.png")
    plt, legend_list = plot_single_operation(avl_extern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "AVL externalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/extern/avl_extern_delete.png")
    plt, legend_list = plot_single_operation(avl_extern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "AVL externalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/extern/avl_extern_lookup.png")
    ###   BST/AVL per operation
    _, legend_list_bst = plot_single_operation(bst_extern_compilation_times_df, "INSERT", True)
    plt, legend_list_avl = plot_single_operation(avl_extern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST/AVL: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/extern/bst_vs_avl_extern_insert.png")
    _, legend_list_bst = plot_single_operation(bst_extern_compilation_times_df, "DELETE", True)
    plt, legend_list_avl = plot_single_operation(avl_extern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST/AVL: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/extern/bst_vs_avl_extern_delete.png")
    _, legend_list_bst = plot_single_operation(bst_extern_compilation_times_df, "LOOKUP", True)
    plt, legend_list_avl = plot_single_operation(avl_extern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "BST/AVL: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/extern/bst_vs_avl_extern_lookup.png")

    ##### Intern
    ###   BST
    plt, legend_list = plot_single_operation(bst_intern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST internalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/intern/bst_intern_insert.png")
    plt, legend_list = plot_single_operation(bst_intern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST internalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/intern/bst_intern_delete.png")
    plt, legend_list = plot_single_operation(bst_intern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "BST internalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/intern/bst_intern_lookup.png")
    ###   AVL
    plt, legend_list = plot_single_operation(avl_intern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "AVL internalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/intern/avl_intern_insert.png")
    plt, legend_list = plot_single_operation(avl_intern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "AVL internalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/intern/avl_intern_delete.png")
    plt, legend_list = plot_single_operation(avl_intern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "AVL internalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "plots/intern/avl_intern_lookup.png")
    ###   BST/AVL per operation
    _, legend_list_bst = plot_single_operation(bst_intern_compilation_times_df, "INSERT", True)
    plt, legend_list_avl = plot_single_operation(avl_intern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST/AVL: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/intern/bst_vs_avl_intern_insert.png")
    _, legend_list_bst = plot_single_operation(bst_intern_compilation_times_df, "DELETE", True)
    plt, legend_list_avl = plot_single_operation(avl_intern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST/AVL: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/intern/bst_vs_avl_intern_delete.png")
    _, legend_list_bst = plot_single_operation(bst_intern_compilation_times_df, "LOOKUP", True)
    plt, legend_list_avl = plot_single_operation(avl_intern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "BST/AVL: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst, legend_list_avl))
    save_plot(plt, "plots/intern/bst_vs_avl_intern_lookup.png")

    ##### Extern/Intern (BST)
    ###   Insert
    _, legend_list_bst1 = plot_single_operation(bst_extern_compilation_times_df, "INSERT", True)
    plt, legend_list_bst2 = plot_single_operation(bst_intern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "Externalista/Internalista (BST): inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst1, legend_list_bst2))
    save_plot(plt, "plots/bst_extern_vs_bst_intern_insert.png")
    ###   Delete
    _, legend_list_bst1 = plot_single_operation(bst_extern_compilation_times_df, "DELETE", True)
    plt, legend_list_bst2 = plot_single_operation(bst_intern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "Externalista/Internalista (BST): borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst1, legend_list_bst2))
    save_plot(plt, "plots/bst_extern_vs_bst_intern_delete.png")
    ###   Lookup
    _, legend_list_bst1 = plot_single_operation(bst_extern_compilation_times_df, "LOOKUP", True)
    plt, legend_list_bst2 = plot_single_operation(bst_intern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "Externalista/Internalista (BST): búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst1, legend_list_bst2))
    save_plot(plt, "plots/bst_extern_vs_bst_intern_lookup.png")

    ##### Extern/Intern (AVL)
    ###   Insert
    _, legend_list_avl1 = plot_single_operation(avl_extern_compilation_times_df, "INSERT", True)
    plt, legend_list_avl2 = plot_single_operation(avl_intern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "Externalista/Internalista (AVL): inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_avl1, legend_list_avl2))
    save_plot(plt, "plots/avl_extern_vs_avl_intern_insert.png")
    ###   Delete
    _, legend_list_avl1 = plot_single_operation(avl_extern_compilation_times_df, "DELETE", True)
    plt, legend_list_avl2 = plot_single_operation(avl_intern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "Externalista/Internalista (AVL): borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_avl1, legend_list_avl2))
    save_plot(plt, "plots/avl_extern_vs_avl_intern_delete.png")
    ###   Lookup
    _, legend_list_avl1 = plot_single_operation(avl_extern_compilation_times_df, "LOOKUP", True)
    plt, legend_list_avl2 = plot_single_operation(avl_intern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "Externalista/Internalista (AVL): búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_avl1, legend_list_avl2))
    save_plot(plt, "plots/avl_extern_vs_avl_intern_lookup.png")

    ##### Fullextern/Extern/Intern (BST)
    ###   Insert
    _, legend_list_bst1 = plot_single_operation(bst_fullextern_compilation_times_df, "INSERT", True)
    _, legend_list_bst2 = plot_single_operation(bst_extern_compilation_times_df, "INSERT", True)
    plt, legend_list_bst3 = plot_single_operation(bst_intern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "Compl. Extern./Externalista/Internalista (BST): inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst1, legend_list_bst2, legend_list_bst3))
    save_plot(plt, "plots/bst_fullextern_vs_extern_vs_intern_insert.png")
    ###   Delete
    _, legend_list_bst1 = plot_single_operation(bst_fullextern_compilation_times_df, "DELETE", True)
    _, legend_list_bst2 = plot_single_operation(bst_extern_compilation_times_df, "DELETE", True)
    plt, legend_list_bst3 = plot_single_operation(bst_intern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "Compl. Extern./Externalista/Internalista (BST): borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst1, legend_list_bst2, legend_list_bst3))
    save_plot(plt, "plots/bst_fullextern_vs_extern_vs_intern_delete.png")
    ###   Lookup
    _, legend_list_bst1 = plot_single_operation(bst_fullextern_compilation_times_df, "LOOKUP", True)
    _, legend_list_bst2 = plot_single_operation(bst_extern_compilation_times_df, "LOOKUP", True)
    plt, legend_list_bst3 = plot_single_operation(bst_intern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "Compl. Extern./Externalista/Internalista (BST): búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_bst1, legend_list_bst2, legend_list_bst3))
    save_plot(plt, "plots/bst_fullextern_vs_extern_vs_intern_lookup.png")

    ##### Fullextern/Extern/Intern (AVL)
    ###   Insert
    _, legend_list_avl1 = plot_single_operation(avl_fullextern_compilation_times_df, "INSERT", True)
    _, legend_list_avl2 = plot_single_operation(avl_extern_compilation_times_df, "INSERT", True)
    plt, legend_list_avl3 = plot_single_operation(avl_intern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "Compl. Extern./Externalista/Internalista (AVL): inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_avl1, legend_list_avl2, legend_list_avl3))
    save_plot(plt, "plots/avl_fullextern_vs_extern_vs_intern_insert.png")
    ###   Delete
    _, legend_list_avl1 = plot_single_operation(avl_fullextern_compilation_times_df, "DELETE", True)
    _, legend_list_avl2 = plot_single_operation(avl_extern_compilation_times_df, "DELETE", True)
    plt, legend_list_avl3 = plot_single_operation(avl_intern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "Compl. Extern./Externalista/Internalista (AVL): borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_avl1, legend_list_avl2, legend_list_avl3))
    save_plot(plt, "plots/avl_fullextern_vs_extern_vs_intern_delete.png")
    ###   Lookup
    _, legend_list_avl1 = plot_single_operation(avl_fullextern_compilation_times_df, "LOOKUP", True)
    _, legend_list_avl2 = plot_single_operation(avl_extern_compilation_times_df, "LOOKUP", True)
    plt, legend_list_avl3 = plot_single_operation(avl_intern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "Compl. Extern./Externalista/Internalista (AVL): búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list_avl1, legend_list_avl2, legend_list_avl3))
    save_plot(plt, "plots/avl_fullextern_vs_extern_vs_intern_lookup.png")
