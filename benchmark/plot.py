from pandas import DataFrame, Series, concat
from seaborn import scatterplot, lineplot
from matplotlib.pyplot import savefig
from scipy.optimize import curve_fit
from numpy import exp2, log2, arange, square, array


bst_unsafe_running_times_df = DataFrame({
    "bench_name": Series(["bst_unsafe" for _ in range(10 * 3)], dtype="category"),
    "bench_type": Series(["run" for _ in range(10 * 3)], dtype="category"),
    "N": Series([16 for _ in range(10 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in [pow(2, i) for i in range(6, 16, 1)]], dtype="int32"),
    "time": Series([9.414e-05, 2.987e-04, 9.017e-04, 2.370e-03, 1.000e-02, 4.125e-02, 1.877e-01, 8.985e-01, 4.841e+00, 4.216e+01, 5.021e-05, 1.415e-04, 1.080e-03, 6.973e-03, 1.705e-02, 4.967e-02, 1.984e-01, 9.229e-01, 5.033e+00, 4.185e+01, 1.063e-05, 9.998e-06, 1.742e-05, 3.196e-05, 5.698e-05, 1.173e-04, 1.796e-04, 2.942e-04, 4.871e-04, 9.392e-04], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(10)], dtype="category")
})

avl_unsafe_running_times_df = DataFrame({
    "bench_name": Series(["avl_unsafe" for _ in range(10 * 3)], dtype="category"),
    "bench_type": Series(["run" for _ in range(10 * 3)], dtype="category"),
    "N": Series([16 for _ in range(10 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in [pow(2, i) for i in range(6, 16, 1)]], dtype="int32"),
    "time": Series([1.057e-05, 1.066e-05, 1.656e-05, 3.134e-05, 5.720e-05, 1.232e-04, 2.146e-04, 4.395e-04, 8.596e-04, 1.664e-03, 6.922e-06, 9.065e-06, 1.589e-05, 2.940e-05, 5.643e-05, 1.167e-04, 2.143e-04, 4.184e-04, 8.393e-04, 1.669e-03, 2.588e-06, 1.618e-06, 1.457e-06, 1.581e-06, 1.405e-06, 1.681e-06, 1.381e-06, 1.447e-06, 1.467e-06, 1.422e-06], dtype="float32"),
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
    "time": Series([2.41, 2.86, 3.52, 4.64, 6.09, 7.63, 10.17, 13.84, 18.09, 22.77, 2.24, 3.47, 4.59, 6.07, 7.69, 10.41, 14.18, 18.62, 23.28, 2.69, 3.33, 4.43, 5.93, 10.06, 13.7, 17.95, 22.56], dtype="float32"),
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

def square_fun(x, a, b):
    return a * square(x) + b

def loglinear_fun(x, a, b, c):
    return (a * x + b) * log2(x) + c

def linear_fun(x, a, b):
    return a * x + b

def log_fun(x, a, b):
    return a * log2(x) + b


def get_fit_fun_params(data, bench_name, op):
    y_max = data[(data.bench_name == bench_name) & (data.operation == op)]["time"].max()
    x_max = data[(data.bench_name == bench_name) & (data.operation == op) & (data.time == y_max)]["size"].iloc[0]
    y_min = data[(data.bench_name == bench_name) & (data.operation == op)]["time"].min()
    x_min = data[(data.bench_name == bench_name) & (data.operation == op) & (data.time == y_min)]["size"].iloc[0]

    if ("fullextern" in bench_name):
        fun = exp_fun
        fun_legend = f"Ajuste exponencial ({op.capitalize()})"
        p0 = (1, - log2(y_max) / x_max, 0)
    elif ((bench_name == "bst_extern") or (bench_name == "bst_intern")):
        fun = exp_fun
        fun_legend = f"Ajuste exponencial ({op.capitalize()})"
        p0 = (1, - log2(y_max) / x_max, 0)
    elif((bench_name == "avl_extern") or (bench_name == "avl_intern")):
        fun = linear_fun
        fun_legend = f"Ajuste lineal ({op.capitalize()})"
        b = (y_min * x_max - y_max * x_min) / (x_max - x_min)
        a = (y_max - b) / x_max
        p0 = (a, b)
    elif(bench_name == "bst_unsafe"):
        if (op.lower() == "lookup"):
            fun = linear_fun
            fun_legend = f"Ajuste lineal ({op.capitalize()})"
            b = (y_min * x_max - y_max * x_min) / (x_max - x_min)
            a = (y_max - b) / x_max
            p0 = (a, b)
        else:
            fun = exp_fun
            fun_legend = f"Ajuste exponencial ({op.capitalize()})"
            p0 = (1, - log2(y_max) / x_max, 0)
    else:   # (bench_name == "avl_unsafe")
        if (op.lower() == "lookup"):
            fun = linear_fun
            fun_legend = f"Ajuste lineal ({op.capitalize()})"
            b = (y_min * x_max - y_max * x_min) / (x_max - x_min)
            a = (y_max - b) / x_max
            p0 = (a, b)
        else:
            fun = square_fun
            fun_legend = f"Ajuste cuadrático ({op.capitalize()})"
            a = (y_max - y_min) / (square(x_max) - square(x_min))
            b = y_max - a * square(x_max)
            p0 = (a, b)
    
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

    if (fit):
        bench_name = data["bench_name"].iloc[0]
        fun, fun_legend, p0 = get_fit_fun_params(data, bench_name, op)

        popt, ss, max_s = fit_function(op_data, fun, p0)
        y_fit = list(map(lambda x: fun(x, *popt), x_fit))

        plt = lineplot(x=x_fit, y=y_fit)
        fun_legend += f" - ss={'{:.2f}'.format(ss)}/max_s={'{:.2f}'.format(max_s)}"

        return plt, ([fun_legend], [op.capitalize()])

    return plt, ([], [op.capitalize()])

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
    plt, legend_list = plot_single_operation(bst_unsafe_running_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST no seguro: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_unsafe_insert.png")
    plt, legend_list = plot_single_operation(bst_unsafe_running_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST no seguro: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_unsafe_delete.png")
    plt, legend_list = plot_single_operation(bst_unsafe_running_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "BST no seguro: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_unsafe_lookup.png")

    plt, legend_list = plot_single_operation(avl_unsafe_running_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "AVL no seguro: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_unsafe_insert.png")
    plt, legend_list = plot_single_operation(avl_unsafe_running_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "AVL no seguro: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_unsafe_delete.png")
    plt, legend_list = plot_single_operation(avl_unsafe_running_times_df, "LOOKUP", False)
    plt = set_plot_labels(plt, "AVL no seguro: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_unsafe_lookup.png")

    plt, legend_list = plot_single_operation(bst_fullextern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST completamente externalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_fullextern_insert.png")
    plt, legend_list = plot_single_operation(bst_fullextern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST completamente externalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_fullextern_delete.png")
    plt, legend_list = plot_single_operation(bst_fullextern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "BST completamente externalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_fullextern_lookup.png")

    plt, legend_list = plot_single_operation(avl_fullextern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "AVL completamente externalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_fullextern_insert.png")
    plt, legend_list = plot_single_operation(avl_fullextern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "AVL completamente externalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_fullextern_delete.png")
    plt, legend_list = plot_single_operation(avl_fullextern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "AVL completamente externalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_fullextern_lookup.png")

    plt, legend_list = plot_single_operation(bst_extern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST externalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_extern_insert.png")
    plt, legend_list = plot_single_operation(bst_extern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST externalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_extern_delete.png")
    plt, legend_list = plot_single_operation(bst_extern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "BST externalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_extern_lookup.png")

    plt, legend_list = plot_single_operation(avl_extern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "AVL externalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_extern_insert.png")
    plt, legend_list = plot_single_operation(avl_extern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "AVL externalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_extern_delete.png")
    plt, legend_list = plot_single_operation(avl_extern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "AVL externalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_extern_lookup.png")

    plt, legend_list = plot_single_operation(bst_intern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "BST internalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_intern_insert.png")
    plt, legend_list = plot_single_operation(bst_intern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "BST internalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_intern_delete.png")
    plt, legend_list = plot_single_operation(bst_intern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "BST internalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "bst_intern_lookup.png")

    plt, legend_list = plot_single_operation(avl_intern_compilation_times_df, "INSERT", True)
    plt = set_plot_labels(plt, "AVL internalista: inserción", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_intern_insert.png")
    plt, legend_list = plot_single_operation(avl_intern_compilation_times_df, "DELETE", True)
    plt = set_plot_labels(plt, "AVL internalista: borrado", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_intern_delete.png")
    plt, legend_list = plot_single_operation(avl_intern_compilation_times_df, "LOOKUP", True)
    plt = set_plot_labels(plt, "AVL internalista: búsqueda", "Tamaño del árbol", "Tiempo (seg)", _combine_legend_lists(legend_list))
    save_plot(plt, "avl_intern_lookup.png")
