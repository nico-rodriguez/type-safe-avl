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
    "time": Series([3.707e-04, 9.402e-04, 2.335e-03, 9.007e-03, 3.165e-02, 1.232e-01, 4.844e-01, 1.920e+00, 7.803e+00, 3.237e+01, 2.823e-04, 1.036e-03, 4.062e-03, 1.558e-02, 6.067e-02, 2.409e-01, 9.509e-01, 3.804e+00, 1.532e+01, 6.262e+01, 6.118e-06, 2.518e-06, 2.605e-06, 2.699e-06, 2.316e-06, 2.876e-06, 2.881e-06, 2.979e-06, 2.962e-06, 2.430e-06], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(10)], dtype="category")
})

bst_fullextern_compilation_times_df = DataFrame({
    "bench_name": Series(["bst_fullextern" for _ in range(10 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(10 * 3)], dtype="category"),
    "N": Series([16 for _ in range(10 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 110, 10)], dtype="int8"),
    "time": Series([7.52, 8.05, 8.67, 9.77, 11.13, 13.07, 15.27, 18.31, 21.85, 26.05, 7.5, 8.04, 8.66, 9.79, 11.07, 12.99, 15.27, 18.29, 21.91, 25.86, 7.49, 7.99, 8.69, 9.87, 11.07, 12.89, 15.3, 18.22, 21.79, 25.61], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(10)], dtype="category")
})

avl_fullextern_compilation_times_df = DataFrame({
    "bench_name": Series(["avl_fullextern" for _ in range(10 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(10 * 3)], dtype="category"),
    "N": Series([16 for _ in range(10 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 110, 10)], dtype="int8"),
    "time": Series([8.03, 8.23, 8.72, 9.88, 11.37, 12.96, 15.57, 19.25, 23.49, 28.13, 7.74, 8.42, 9.42, 11.36, 13.7, 16.27, 20.31, 26.96, 32.87, 41.83, 7.77, 8.16, 8.77, 9.88, 11.4, 12.9, 15.49, 19.21, 23.4, 28.02], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(10)], dtype="category")
})

bst_extern_compilation_times_df = DataFrame({
    "bench_name": Series(["bst_extern" for _ in range(5 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(5 * 3)], dtype="category"),
    "N": Series([4 for _ in range(5 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 60, 10)], dtype="int8"),
    "time": Series([8.09, 12.09, 22.95, 46.94, 90.31, 8.31, 11.69, 21.85, 44.67, 85.48, 8.17, 11.86, 21.82, 44.52, 85.7], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(5)], dtype="category")
})

avl_extern_compilation_times_df = DataFrame({
    "bench_name": Series(["avl_extern" for _ in range(6 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(6 * 3)], dtype="category"),
    "N": Series([16 for _ in range(6 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 70, 10)], dtype="int8"),
    "time": Series([7.98, 9.61, 11.74, 15.47, 19.73, 24.25, 8.38, 10.69, 13.97, 19.54, 26.56, 33.75, 8.17, 9.66, 11.66, 15.07, 19.26, 23.71], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(6)], dtype="category")
})

bst_intern_compilation_times_df = DataFrame({
    "bench_name": Series(["bst_intern" for _ in range(5 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(5 * 3)], dtype="category"),
    "N": Series([4 for _ in range(5 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 60, 10)], dtype="int8"),
    "time": Series([7.64, 11.15, 21.36, 43.91, 86.49, 7.81, 11.18, 20.61, 42.44, 81.64, 8.34, 11.07, 20.61, 42.38, 81.84], dtype="float32"),
    "operation": Series([op for op in ["INSERT", "DELETE", "LOOKUP"] for _ in range(5)], dtype="category")
})

avl_intern_compilation_times_df = DataFrame({
    "bench_name": Series(["avl_intern" for _ in range(6 * 3)], dtype="category"),
    "bench_type": Series(["compile" for _ in range(6 * 3)], dtype="category"),
    "N": Series([16 for _ in range(6 * 3)], dtype="int8"),
    "size": Series([size for _ in range(3) for size in range(10, 70, 10)], dtype="int8"),
    "time": Series([7.88, 8.72, 10.2, 12.46, 15.34, 18.58, 7.98, 9.41, 11.29, 15.19, 19.7, 24.52, 7.85, 8.7, 10.12, 12.33, 15.16, 18.23], dtype="float32"),
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
