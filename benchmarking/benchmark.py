import os
import shutil
import subprocess
import csv
import matplotlib.pyplot as plt
import numpy as np
from collections import defaultdict
import json
import extract_errors
from dataclasses import dataclass


def write_empty_json_if_not_exists(path : str) -> None:
    if not os.path.exists(path):
        with open(path, "w") as file:
            file.write("{}")

def update_json_file(path : str, new_dict : dict) -> None:
    with open(path, "r") as existing_file:
        existing_data = json.loads(existing_file.read())

    for key, val in new_dict.items():
        existing_data[key] = val

    with open(path, "w") as existing_file:
        json.dump(existing_data, existing_file, indent=4, ensure_ascii=False)

'''
given the name of a benchmark, who has a corresponding .ml file, this function will generate
executables for Moscow ML, MLTon, and PolyML in the 'out' folder
(writing to files suffixed with with _mlton, _mosml, and _polyml)
'''
def generate_sml_executables(benchmark_name : str, number_runs : int) -> None:

    if os.path.exists("benchmarking/tmp"):
        shutil.rmtree("benchmarking/tmp")
    os.mkdir("benchmarking/tmp")

    with open(f"benchmarking/programs/{benchmark_name}.sml", "r") as file:
        prog = file.read()


    mlton_program = prog + "\n" + f"val _ = Main.doit {number_runs}"
    with open(f"benchmarking/tmp/{benchmark_name}_mlton.sml", "w") as file:
        file.write(mlton_program)

    subprocess.run(["mlton", "-output", f"benchmarking/out/{benchmark_name}.mlton", f"benchmarking/tmp/{benchmark_name}_mlton.sml"])


    mosml_program = prog + "\n" + f"val _ = Main.doit {number_runs}"
    with open(f"benchmarking/tmp/{benchmark_name}_mosml.sml", "w") as file:
        file.write(mosml_program)

    subprocess.run(["mosmlc", "-orthodox", "-standalone", "-toplevel", "-o", f"benchmarking/out/{benchmark_name}.mosml", f"benchmarking/tmp/{benchmark_name}_mosml.sml"])

    polyc_program = prog + "\n" + f"fun main () = Main.doit {number_runs}"
    with open(f"benchmarking/tmp/{benchmark_name}_polyc.sml", "w") as file:
        file.write(polyc_program)

    subprocess.run(["polyc", "-o", f"benchmarking/out/{benchmark_name}.polyc", f"benchmarking/tmp/{benchmark_name}_polyc.sml"])

    smlnj_program = "local \nval _ = SMLofNJ.Internals.GC.messages false\n" + prog + "\n in\n" + \
    f"val _ = SMLofNJ.exportFn (\"{benchmark_name}\", fn _ => (Main.doit {number_runs}; OS.Process.success))\n" + \
    "end"
    with open(f"benchmarking/tmp/{benchmark_name}_smlnj.sml", "w") as file:
        file.write(smlnj_program)

    subprocess.run(["sml", f"benchmarking/tmp/{benchmark_name}_smlnj.sml"])
    subprocess.run(["mv", f"{benchmark_name}.amd64-darwin", f"benchmarking/out/{benchmark_name}.smlnj"])


    shutil.rmtree("benchmarking/tmp")

'''
rewrite from hyperfile format (a list of objects that each contain a 'command')
field with the name, to a single object with mapping from command ->

e.g. [ {command : "mosml", time : 5.0}, {command : "mlton", time : 1.0}]
-->
{
    "mosml" : {time : 5.0},
    "mlton" : {time : 1.0}
}

useful for overwriting the results for a single compiler
'''
def rewrite_hyperfile_out_json(plot_name : str, benchmark_name : str) -> None:
    with open(f"benchmarking/results/{benchmark_name}_tmp.json", "r") as tmp_file:
        tmp_data = json.loads(tmp_file.read())

    modified_json = {}

    for compiler_results in tmp_data["results"]:
        command = compiler_results["command"]
        del compiler_results["command"]
        compiler = command.split(".")[1]
        modified_json[compiler] = compiler_results

    result_file = f"benchmarking/results/{plot_name}/{benchmark_name}.json"

    write_empty_json_if_not_exists(result_file)

    update_json_file(result_file, modified_json)

'''

'''
def create_result_directories_if_not_exist(plot_name : str) -> None:
    if not os.path.exists("benchmarking/results"):
        os.mkdir("benchmarking/results")

    if not os.path.exists(f"benchmarking/results/{plot_name}"):
        os.mkdir(f"benchmarking/results/{plot_name}")

    if not os.path.exists("benchmarking/out"):
        os.mkdir("benchmarking/out")

    if not os.path.exists("benchmarking/jmh/lib"):
        os.mkdir("benchmarking/jmh/lib")

    if not os.path.exists("benchmarking/jmh/src/main/java/jvml/benchmark"):
        os.makedirs("benchmarking/jmh/src/main/java/jvml/benchmark")

'''
given that the executables exist in tmp, run each sml version

for now, just use hyperfine
'''
def run_sml_benchmarking(plot_name : str, benchmark_name : str, warmups=5, runs=10):

    create_result_directories_if_not_exist(plot_name)

    subprocess.run(
        ["hyperfine",
         "--export-json", f"benchmarking/results/{benchmark_name}_tmp.json",
         "--warmup", f"{warmups}", "--runs", f"{runs}",
         f"benchmarking/out/{benchmark_name}.mlton",
         f"benchmarking/out/{benchmark_name}.mosml",
         f"benchmarking/out/{benchmark_name}.polyc",
         f"sml @SMLload=benchmarking/out/{benchmark_name}.smlnj"
    ])

    rewrite_hyperfile_out_json(plot_name, benchmark_name)

    os.remove(f"benchmarking/results/{benchmark_name}_tmp.json")


'''
Clear .java, .jar and .class files from previous benchmarking runs

Required we currently generate code belonging to the same package,
which might lead to conflicting names.
'''
def clear_java_directories() -> None:
    if os.path.exists("benchmarking/jmh/lib"):
        for file in os.listdir("benchmarking/jmh/lib"):
            os.remove("benchmarking/jmh/lib/" + file)

    for file in os.listdir("benchmarking/jmh/src/main/java/jvml/benchmark"):
        os.remove("benchmarking/jmh/src/main/java/jvml/benchmark/" + file)

    if os.path.exists("benchmarking/jmh/target"):
        shutil.rmtree("benchmarking/jmh/target")

ALL_OPTIMISATIONS : list[str] = ["-peep", "-const-fp", "-tco", "-tmm", "-tmc", "-inline"]

def generate_jvml_jar(benchmark_name : str, number_runs : int, optimisation_flags : list[str], result_suffix : str):
    if os.path.exists("benchmarking/tmp"):
        shutil.rmtree("benchmarking/tmp")
    os.mkdir("benchmarking/tmp")

    if not os.path.exists("benchmarking/jmh/lib"):
        os.mkdir("benchmarking/jmh/lib")

    with open(f"benchmarking/programs/{benchmark_name}.jvml", "r") as file:
        prog = file.read()

    jvml_program = prog + "\n" + f"val benchmark = doit {number_runs}"

    with open(f"benchmarking/tmp/{benchmark_name}.jvml", "w") as file:
        file.write(jvml_program)

    class_name = benchmark_name.capitalize() + result_suffix
    out_name = benchmark_name + result_suffix

    subprocess.run(["bash", "scripts/build_jar.sh",
                    "-f", f"benchmarking/tmp/{benchmark_name}.jvml",
                    "-c", class_name] + optimisation_flags
                    )

    subprocess.run(["mv", f"{class_name}.jar", f"benchmarking/jmh/lib/{out_name}.jar"])

    shutil.rmtree("benchmarking/tmp")


def generate_jmh_executables(benchmark_name : str, number_runs : int, optimisation_flags : list[str], result_suffix : str) -> None:

    clear_java_directories()

    generate_jvml_jar(benchmark_name, number_runs, optimisation_flags, result_suffix)

    class_name = benchmark_name.capitalize() + result_suffix
    out_name = benchmark_name + result_suffix



    JMH_CODE = f"""
package jvml.benchmark;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.infra.Blackhole;
import org.openjdk.jmh.results.format.ResultFormatType;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import org.openjdk.jmh.runner.options.TimeValue;

import jvml.generated.{class_name};

import java.util.concurrent.TimeUnit;

public class Benchmark{class_name} {{


    public static void main(String[] args) throws RunnerException {{
        Options opt = new OptionsBuilder()
                .include(Benchmark{class_name}.class.getSimpleName())
                .resultFormat(ResultFormatType.JSON)
                .result("benchmarking/results/{out_name}_tmp.json")
                .forks(3)
                .build();
        new Runner(opt).run();
    }}

    @Benchmark
    @BenchmarkMode(Mode.SampleTime)
    @OutputTimeUnit(TimeUnit.SECONDS)
    @Fork(value = 1, warmups = 1)
    public void bench(Blackhole blackhole) {{
        {class_name}.main(null);
    }}
}}
    """

    with open(f"benchmarking/jmh/src/main/java/jvml/benchmark/Benchmark{class_name}.java", "w") as file:
        file.write(JMH_CODE)


    with open("benchmarking/jmh/pom.xml", "r") as file:
        config_s = file.read()

    config = config_s.split("</dependencies>")

    config[0] += f"""

        <dependency>
            <groupId>jvml.generated</groupId>
            <artifactId>{out_name}</artifactId>
            <version>1.0</version>
            <scope>system</scope>
            <systemPath>${{project.basedir}}/lib/{out_name}.jar</systemPath>
        </dependency>
    """

    with open("benchmarking/jmh/modified_pom.xml", "w") as file:
        file.write(
            "</dependencies>".join(config)
        )

    subprocess.run(["mvn", "clean", "install", "-f", "benchmarking/jmh/modified_pom.xml"])


'''
rewrite jmh's output .json into our unified format
'''
def rewrite_jmh_out_json(plot_name : str, benchmark_name : str, result_suffix : str, result_file : str) -> None:
    out_name = benchmark_name + result_suffix
    with open(f"benchmarking/results/{out_name}_tmp.json", "r") as tmp_file:
        tmp_data = json.loads(tmp_file.read())


    compiler = "jvml" + result_suffix
    data = tmp_data[0]

    modified_json = {
        compiler: data
    }

    write_empty_json_if_not_exists(f"benchmarking/results/{plot_name}/{result_file}.json")

    update_json_file(f"benchmarking/results/{plot_name}/{result_file}.json", modified_json)


'''
given that the executables exist in jmh/src/main/..., run each benchmark
'''
def run_jvml_benchmarking(plot_name : str, benchmark_name : str, result_suffix : str, result_file : str | None = None, jvm_flags : list[str] = []):

    if result_file is None:
        result_file = benchmark_name

    create_result_directories_if_not_exist(plot_name)

    class_name = benchmark_name.capitalize() + result_suffix
    out_name = benchmark_name + result_suffix


    if os.path.exists(f"benchmarking/results/{out_name}_tmp.json"):
        os.remove(f"benchmarking/results/{out_name}_tmp.json")

    subprocess.run([
        "/Users/sam/Library/Java/JavaVirtualMachines/openjdk-21.0.2/Contents/Home/bin/java",
        "-javaagent:/Applications/IntelliJ IDEA CE.app/Contents/lib/idea_rt.jar=64963:/Applications/IntelliJ IDEA CE.app/Contents/bin",
        "-Dfile.encoding=UTF-8",
        "-Dsun.stdout.encoding=UTF-8",
        "-Dsun.stderr.encoding=UTF-8",
        "-Xmx8589934592", "-Xss1g"] + jvm_flags +

        ["-classpath", f"/Users/sam/jvml/benchmarking/jmh/target/classes:/Users/sam/.m2/repository/org/openjdk/jmh/jmh-core/1.37/jmh-core-1.37.jar:/Users/sam/.m2/repository/net/sf/jopt-simple/jopt-simple/5.0.4/jopt-simple-5.0.4.jar:/Users/sam/.m2/repository/org/apache/commons/commons-math3/3.6.1/commons-math3-3.6.1.jar:/Users/jvml/.m2/repository/org/openjdk/jmh/jmh-generator-annprocess/1.37/jmh-generator-annprocess-1.37.jar:/Users/sam/jvml/benchmarking/jmh/lib/{out_name}.jar",
        f"jvml.benchmark.Benchmark{class_name}", "+TieredCompilation"]
    )

    rewrite_jmh_out_json(plot_name, benchmark_name, result_suffix, result_file)

    os.remove(f"benchmarking/results/{out_name}_tmp.json")

'''
given that a .jar file exists in benchmarking/jmh/lib/ named (benchmark_name + result_suffix)

compute it's file size and add the result to the result .json file in our unified format
'''
def compute_jvml_file_size(plot_name : str, benchmark_name : str, result_suffix : str, result_file : str | None = None):
    if result_file is None:
        result_file = benchmark_name

    create_result_directories_if_not_exist(plot_name)

    out_name = benchmark_name + result_suffix
    result_file = f"benchmarking/results/{plot_name}/{result_file}.json"

    file_size = os.path.getsize(f"benchmarking/jmh/lib/{out_name}.jar")

    write_empty_json_if_not_exists(result_file)

    update_json_file(result_file, {"jvml" + result_suffix : {"file size" : file_size}})


'''
todo refactor into one 'get file size method'
'''
def compute_sml_file_sizes(plot_name : str, benchmark_name : str):

    create_result_directories_if_not_exist(plot_name)
    result_file = f"benchmarking/results/{plot_name}/{benchmark_name}.json"
    write_empty_json_if_not_exists(result_file)

    file_sizes = {}
    for compiler in ["mlton", "mosml", "polyc"]:
        file_sizes[compiler] = {
            "file size" : os.path.getsize(f"benchmarking/out/{benchmark_name}.{compiler}")
        }

    update_json_file(result_file, file_sizes)



def parse_result_json(plot_name : str, benchmark_name : str, param_name : str) -> dict[str, int | float]:
    with open(f"benchmarking/results/{plot_name}/{benchmark_name}.json", "r") as file:
        benchmark_results = json.loads(file.read())

    time_by_key = {key : benchmark_results[key][param_name] for key in benchmark_results}

    return time_by_key

@dataclass
class BenchmarkResult:
    mean : float
    error : float


def parse_result_json_with_error(plot_name : str, benchmark_name : str) -> dict[str, BenchmarkResult]:
    with open(f"benchmarking/results/{plot_name}/{benchmark_name}.json", "r") as file:
        benchmark_results = json.loads(file.read())

    def get_stats(key):
        if "jvml" in key:
            return (BenchmarkResult(mean=benchmark_results[key]["primaryMetric"]["score"], error=extract_errors.jmh_extract_mean_error(benchmark_results, key, 0.95)))
        else:
            return (BenchmarkResult(mean=benchmark_results[key]["mean"], error=extract_errors.hyperfine_extract_mean_error(benchmark_results, key, 0.95)))

    time_by_key = {key : get_stats(key) for key in benchmark_results}

    return time_by_key

def save_figure(plot_name : str) -> None:
    if not os.path.exists("benchmarking/figures"):
        os.mkdir("benchmarking/figures")
    plt.savefig(f"benchmarking/figures/{plot_name}.pdf", bbox_inches='tight')

PRETTY_NAMES = {
    "jvml_constant_propagation" : "constant folding & propagation",
    "jvml_peephole" : "peephole",
    "jvml_tco":  "tce",
    "jvml_tail_mod_monoid" : "tail modulo monoid + tce",
    "jvml_tail_mod_cons" : "tail modulo cons + tce",
    "jvml_inline" : "inline",

    "jvml_opt" : "Jvml (optimised)",
    "jvml" : "Jvml",
    "mlton" : "MLton",
    "mosml" : "MosML",
    "polyc" : "Poly/ML",
    "smlnj" : "SML/NJ",

    "jvml_map_tmc" : "tail mod cons",
    "jvml_map_cps_defun" : "CPS+defun",
    "jvml_map_rev" : "APS+rev",
    "jvml_naive_map" : "non-tail-recursive map",

    "jvml_0" : "0",
    "jvml_5" : "5",
    "jvml_10" : "10",

    "jvml_box" : "box/unbox",
    "jvml_push_pop" : "push/pop",
    "jvml_goto_label" : "goto/label",
    "jvml_store_load" : "store/load",

    "short_life" : "life",
    "short_mandlebrot" : "mandlebrot",
    "short_tak" : "tak",

    "jvml_1" : "1",
    "jvml_2" : "2",
    "jvml_3" : "3",
    "jvml_4" : "4",
    "jvml_5" : "5",
    "jvml_6" : "6",
    "jvml_7" : "7",
    "jvml_8" : "8",
    "jvml_9" : "9",
    "jvml_10" : "10",
    "jvml_11" : "11",
    "jvml_12" : "12",
    "jvml_13" : "13",

    "desugaring" : "lowering to bIRch",
    "middle_end" : "optimising",
    "linearise" : "lowering to linearIR",
    "lower_jvm" : "lowering to bytecode"


}

def prettify_names(data):
    res = {}
    for k, v in data.items():
        if k in PRETTY_NAMES:
            res[PRETTY_NAMES[k]] = v
        else:
            res[k] = v
    return res


def make_metrics_relative(result : BenchmarkResult, mean_relative_to : float) -> BenchmarkResult:
    std_error_proportion = result.error / result.mean
    relative_mean = result.mean / mean_relative_to
    relative_error = relative_mean * std_error_proportion
    return BenchmarkResult(mean=relative_mean, error=relative_error)

'''
given a list of benchmarks that have been run (with results as csv files in the 'csv' directory)

produce a bar plot of performance
'''
def plot_comparing_compilers_time(plot_name : str, benchmark_names : list[str]) -> None:
    benchmark_results = {benchmark_name : parse_result_json_with_error(plot_name, benchmark_name) for benchmark_name in benchmark_names}
    ordered_results_by_compiler = defaultdict(list)

    for benchmark_name in benchmark_results:
        for compiler in sorted(benchmark_results[benchmark_name]):
            relative_result = make_metrics_relative(benchmark_results[benchmark_name][compiler], benchmark_results[benchmark_name]["mlton"].mean)

            ordered_results_by_compiler[compiler].append(relative_result)


    ordered_results_by_compiler = prettify_names(ordered_results_by_compiler)

    plt.style.use("seaborn-v0_8-deep")

    _, ax = plt.subplots()

    x = np.arange(len(benchmark_names))
    width = 0.15
    multiplier = 0


    for compiler, results in ordered_results_by_compiler.items():
        execution_time = [r.mean for r in results]
        errors = [r.error for r in results]

        offset = width * multiplier
        ax.bar(x + offset, execution_time, width, yerr=errors, capsize=2, label=compiler, edgecolor='black',linewidth=0.5)
        for i in range(len(execution_time)):
            if execution_time[i] == 0:
                ax.text((x+offset)[i], 1, f"{compiler} failed", rotation="vertical", size=6, ha="center")
        multiplier += 1

    ax.set_xlabel("Benchmark")
    ax.set_ylabel("Execution time relative to mlton")
    ax.set_xticks(x + width * (len(x)+1)/2, [PRETTY_NAMES.get(bn, bn) for bn in benchmark_names])
    ax.legend()

    save_figure(plot_name)


def plot_individual_opts_time(plot_name : str, benchmark_names : list[str]) -> None:
    benchmark_results = {benchmark_name : parse_result_json_with_error(plot_name, benchmark_name) for benchmark_name in benchmark_names}
    ordered_results_by_opt = defaultdict(list)

    for benchmark_name in benchmark_results:
        for optimisation in sorted(benchmark_results[benchmark_name]):
            if optimisation != "jvml_unoptimised":
                relative_result = make_metrics_relative(benchmark_results[benchmark_name][optimisation], benchmark_results[benchmark_name]["jvml_unoptimised"].mean)

                ordered_results_by_opt[optimisation].append(relative_result)

    plt.style.use("seaborn-v0_8-deep")

    _, ax = plt.subplots()

    ordered_results_by_opt = prettify_names(ordered_results_by_opt)

    x = np.arange(len(benchmark_names))
    width = 0.15
    multiplier = 0

    for optimisation, relative_results in ordered_results_by_opt.items():

        relative_execution_time = [r.mean for r in relative_results]
        errors = [r.error for r in relative_results]

        offset = width * multiplier
        ax.bar(x + offset, np.array(relative_execution_time)-1, width, yerr=errors, capsize=2, bottom=1.0, label=optimisation, edgecolor='black',linewidth=0.5)
        for i in range(len(relative_execution_time)):
            if relative_execution_time[i] == 0:
                ax.text((x+offset)[i], 1, f"jvml with only {optimisation} failed", rotation="vertical", size=6, ha="center")
        multiplier += 1

    ax.set_xlabel("Benchmark")

    ax.set_ylabel("Execution time relative to unoptimised jvml")
    ax.set_xticks(x + width * (len(x))/2, [PRETTY_NAMES.get(bn, bn) for bn in benchmark_names])

    ax.spines['bottom'].set_position(('data', 1))
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)


    ax.legend()

    save_figure(plot_name)

def plot_tail_mod_cons_bar_graph(plot_name : str, list_sizes : list[int]):
    benchmark_results = {list_size : parse_result_json_with_error(plot_name, str(list_size)) for list_size in list_sizes}
    ordered_results_by_size = defaultdict(list)
    for benchmark_name in benchmark_results:
        for map_type in benchmark_results[benchmark_name]:
            ordered_results_by_size[map_type].append(benchmark_results[benchmark_name][map_type])

    ordered_results_by_size = prettify_names(ordered_results_by_size)

    plt.style.use("seaborn-v0_8-deep")

    _, ax = plt.subplots()

    x = np.arange(len(list_sizes))
    width = 0.15
    multiplier = 0

    for map_type, results in ordered_results_by_size.items():

        execution_time = [r.mean for r in results]
        errors = [r.error for r in results]

        offset = width * multiplier
        ax.bar(x + offset, execution_time, width, yerr=errors, capsize=2, label=map_type, edgecolor='black',linewidth=0.5)
        for i in range(len(execution_time)):
            if execution_time[i] == 0:
                ax.text((x+offset)[i], 1, f"{map_type} failed", rotation="vertical", size=6, ha="center")
        multiplier += 1

    ax.set_yscale("log")
    plt.xticks(rotation=90)
    ax.set_xlabel("list size")
    ax.set_ylabel("Execution time (seconds)")

    ax.set_xticks(x + width * (len(x)+1)/2, list_sizes)

    ax.legend()

    save_figure(plot_name)

def plot_tail_mod_cons_graph(plot_name : str, list_sizes : list[int]) -> None:
    benchmark_results = {list_size : parse_result_json_with_error(plot_name, str(list_size)) for list_size in list_sizes}
    ordered_results_by_list_size = defaultdict(list)
    for benchmark_name in benchmark_results:
            for map_type in benchmark_results[benchmark_name]:
                relative_result = make_metrics_relative(benchmark_results[benchmark_name][map_type], benchmark_results[benchmark_name]["jvml_naive_map"].mean)

                ordered_results_by_list_size[map_type].append(relative_result)

    ordered_results_by_list_size = prettify_names(ordered_results_by_list_size)

    plt.style.use("seaborn-v0_8-deep")

    _, ax = plt.subplots()
    for map_type, relative_results in ordered_results_by_list_size.items():

        relative_execution_time = [r.mean for r in relative_results]
        errors = [r.error for r in relative_results]

        #ax.plot(list_sizes, relative_execution_time, label=map_type)
        ax.errorbar(list_sizes, relative_execution_time, yerr=errors, capsize=2, label=map_type)

    ax.set_xscale("log")

    ax.set_xlabel("List Size")
    ax.set_ylabel("Execution time relative to naive non tail recursive map")

    ax.legend()

    save_figure(plot_name)

def plot_individual_opts_code_size(plot_name : str, benchmark_names : list[str]) -> None:

    benchmark_results = {benchmark_name : parse_result_json(plot_name, benchmark_name, "file size") for benchmark_name in benchmark_names}
    ordered_benchmarks_by_opt = defaultdict(list)
    for benchmark_name in benchmark_results:
        for optimisation in sorted(benchmark_results[benchmark_name]):
            if optimisation != "jvml_unoptimised":
                ordered_benchmarks_by_opt[optimisation].append(benchmark_results[benchmark_name][optimisation] / benchmark_results[benchmark_name]["jvml_unoptimised"])

    ordered_benchmarks_by_opt = prettify_names(ordered_benchmarks_by_opt)

    plt.style.use("seaborn-v0_8-deep")

    _, ax = plt.subplots()

    x = np.arange(len(benchmark_names))
    width = 0.15
    multiplier = 0

    for optimisation, relative_execution_time in ordered_benchmarks_by_opt.items():
        offset = width * multiplier
        ax.bar(x + offset, np.array(relative_execution_time)-1, width, bottom=1.0, label=optimisation, edgecolor='black',linewidth=0.5)
        for i in range(len(relative_execution_time)):
            if relative_execution_time[i] == 0:
                ax.text((x+offset)[i], 1, f"jvml with only {optimisation} failed", rotation="vertical", size=6, ha="center")
        multiplier += 1

    ax.set_xlabel("Benchmark")

    ticks = np.arange(0.9, 1.22, 0.02)
    ax.set_yticks(ticks)
    ax.set_ylabel("Code size relative to unoptimised jvml")
    ax.set_xticks(x + width * (len(x))/2, benchmark_names)
    ax.tick_params(axis='x', pad=10)

    ax.spines['bottom'].set_position(('data', 1))
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

    ax.legend()

    save_figure(plot_name)

def plot_compiler_size(plot_name : str, benchmark_names : list[str]) -> None:
    benchmark_results = {benchmark_name : parse_result_json(plot_name, benchmark_name, "file size") for benchmark_name in benchmark_names}
    ordered_benchmarks_by_compiler = defaultdict(list)
    for benchmark_name in benchmark_results:
        for compiler in sorted(benchmark_results[benchmark_name]):
            ordered_benchmarks_by_compiler[compiler].append(benchmark_results[benchmark_name][compiler])

    ordered_benchmarks_by_compiler = prettify_names(ordered_benchmarks_by_compiler)

    plt.style.use("seaborn-v0_8-deep")

    _, ax = plt.subplots()

    x = np.arange(len(benchmark_names))
    width = 0.15
    multiplier = 0

    for compiler, relative_execution_time in ordered_benchmarks_by_compiler.items():
        offset = width * multiplier
        ax.bar(x + offset, relative_execution_time, width, label=compiler, edgecolor='black',linewidth=0.5)
        multiplier += 1

    ax.set_xlabel("Benchmark")

    ax.set_ylabel("Code size (bytes)")
    ax.set_xticks(x + width * (len(x)-1)/2, benchmark_names)

    ax.legend()

    save_figure(plot_name)

def plot_varied_inlining_threshold(plot_name : str, benchmark_names : list[str]) -> None:
    benchmark_results = {benchmark_name : parse_result_json_with_error(plot_name, benchmark_name) for benchmark_name in benchmark_names}
    ordered_results_by_threshold = defaultdict(list)
    for benchmark_name in benchmark_results:
        for threshold in benchmark_results[benchmark_name]:
            if threshold != "jvml_no_inlining":
                relative_result = make_metrics_relative(benchmark_results[benchmark_name][threshold], benchmark_results[benchmark_name]["jvml_no_inlining"].mean)
                ordered_results_by_threshold[threshold].append(relative_result)

    ordered_results_by_threshold = prettify_names(ordered_results_by_threshold)

    plt.style.use("seaborn-v0_8-deep")

    _, ax = plt.subplots()

    x = np.arange(len(benchmark_names))  # the label locations
    width = 0.15  # the width of the bars
    multiplier = 0

    for threshold, results in ordered_results_by_threshold.items():

        execution_time = [r.mean for r in results]
        errors = [r.error for r in results]

        offset = width * multiplier
        ax.bar(x + offset, np.array(execution_time)-1, width, bottom=1.0, yerr=errors, capsize=2, label=threshold, edgecolor='black',linewidth=0.5)
        multiplier += 1

    ax.set_xlabel("Benchmark")

    ax.set_ylabel("Execution time relative to no inlining")
    ax.set_xticks(x + width * (len(x))/2, [PRETTY_NAMES.get(bn, bn) for bn in benchmark_names])
    #ax.axhline(y=1, color='r', linestyle='--')
    #ax.set_ylim((0.8, 1.1))

    ax.legend()

    save_figure(plot_name)


def plot_specialised_peephole(plot_name : str, benchmark_names : list[str]) -> None:
    benchmark_results = {benchmark_name : parse_result_json_with_error(plot_name, benchmark_name) for benchmark_name in benchmark_names}
    ordered_results_by_mode = defaultdict(list)
    for benchmark_name in sorted(benchmark_results):
        for mode in benchmark_results[benchmark_name]:
            if mode == "jvml_interpreted_opt":
                relative_result = make_metrics_relative(benchmark_results[benchmark_name][mode], benchmark_results[benchmark_name]["jvml_interpreted_unoptimised"].mean)
                ordered_results_by_mode[mode].append(relative_result)
            if mode == "jvml_jit_opt":
                relative_result = make_metrics_relative(benchmark_results[benchmark_name][mode], benchmark_results[benchmark_name]["jvml_jit_unoptimised"].mean)
                ordered_results_by_mode[mode].append(relative_result)

    ordered_results_by_mode = prettify_names(ordered_results_by_mode)

    plt.style.use("seaborn-v0_8-deep")

    _, ax = plt.subplots()

    x = np.arange(len(benchmark_names))
    width = 0.15
    multiplier = 0

    for mode, relative_results in ordered_results_by_mode.items():

        relative_execution_time = [r.mean for r in relative_results]
        errors = [r.error for r in relative_results]

        offset = width * multiplier
        ax.bar(x + offset, np.array(relative_execution_time)-1, width, yerr=errors, capsize=2, bottom=1.0, label=mode, edgecolor='black',linewidth=0.5)
        multiplier += 1

    ax.set_xlabel("Peephole")

    ax.set_ylabel("Execution Time Relative to (interpreted/jitted) Unoptimised Execution")
    ax.set_xticks(x + width * ((len(ordered_results_by_mode.keys()))/2-1/2), benchmark_names)

    ax.spines['bottom'].set_position(('data', 1))
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

    ax.legend()

    #plt.show()
    save_figure(plot_name)



benchmark_details = [
    ("life", 1),
    ("tak", 1),
    ("mandlebrot", 1),
    ("quicksort", 100),
    ("brzozowski", 1000)
    ]

quick_benchmark_details = [
    ("short_life", 1),
    ("short_tak", 1),
    ("short_mandlebrot", 1),
    ("quicksort", 10),
    ("brzozowski", 100)
]

def compare_compilers_time():
    plot_name = "comparing_compilers_time"
    for benchmark_name, number_of_runs in benchmark_details:
        generate_sml_executables(benchmark_name, number_of_runs)
        run_sml_benchmarking(plot_name, benchmark_name, warmups=10, runs=20) #lazy for now
        generate_jmh_executables(benchmark_name, number_of_runs, ALL_OPTIMISATIONS, "_opt")
        run_jvml_benchmarking(plot_name, benchmark_name, "_opt")
        generate_jmh_executables(benchmark_name, number_of_runs, [], "")
        run_jvml_benchmarking(plot_name, benchmark_name, "")

    #plot_peformance_graphs(plot_name, [b[0] for b in benchmark_details])

def compare_individual_opt_time():
    plot_name = "individual_opt_time_short"
    for benchmark_name, number_of_runs in quick_benchmark_details:

        generate_jmh_executables(benchmark_name, number_of_runs, [], "_unoptimised")
        run_jvml_benchmarking(plot_name, benchmark_name, "_unoptimised")

        generate_jmh_executables(benchmark_name, number_of_runs, ["-const-fp"], "_constant_propagation")
        run_jvml_benchmarking(plot_name, benchmark_name, "_constant_propagation")

        generate_jmh_executables(benchmark_name, number_of_runs, ["-peep"], "_peephole")
        run_jvml_benchmarking(plot_name, benchmark_name, "_peephole")

        generate_jmh_executables(benchmark_name, number_of_runs, ["-tco"], "_tco")
        run_jvml_benchmarking(plot_name, benchmark_name, "_tco")

        generate_jmh_executables(benchmark_name, number_of_runs, ["-tmm", "-tco"], "_tail_mod_monoid")
        run_jvml_benchmarking(plot_name, benchmark_name, "_tail_mod_monoid")

        generate_jmh_executables(benchmark_name, number_of_runs, ["-tmc", "-tco"], "_tail_mod_cons")
        run_jvml_benchmarking(plot_name, benchmark_name, "_tail_mod_cons")

        generate_jmh_executables(benchmark_name, number_of_runs, ["-inline"], "_inline")
        run_jvml_benchmarking(plot_name, benchmark_name, "_inline")

    #plot_individual_opts_graphs(plot_name, [b[0] for b in benchmark_details])

def compare_tail_mod_cons():
    plot_name = "tail_mod_cons"
    #list_sizes = [1, 3, 6, 10, 33, 66, 100, 333, 666, 1000, 3333, 6666, 10_000, 33_333, 66_666, 100_000, 333_333, 666_666, 1_000_000]
    list_sizes = [1, 3, 6, 10, 33, 66, 100]
    for list_size in list_sizes:
        generate_jmh_executables("map", list_size, ["-tmc", "-tco"], f"_map_tmc")
        run_jvml_benchmarking(plot_name, "map", f"_map_tmc", result_file=str(list_size))

        generate_jmh_executables("map_cps_defun", list_size, ["-tco"], f"_map_cps_defun")
        run_jvml_benchmarking(plot_name, "map_cps_defun", f"_map_cps_defun", result_file=str(list_size))

        generate_jmh_executables("map_rev", list_size, ["-tco"], f"_map_rev")
        run_jvml_benchmarking(plot_name, "map_rev", f"_map_rev", result_file=str(list_size))

        generate_jmh_executables("map", list_size, ["-tco"], f"_naive_map")
        run_jvml_benchmarking(plot_name, "map", f"_naive_map", result_file=str(list_size))

    #plot_tail_mod_cons_graph(plot_name, list_sizes)


def compare_individual_opt_size():
    plot_name = "individual_opt_size"
    for benchmark_name, number_of_runs in benchmark_details:
        generate_jvml_jar(benchmark_name, number_of_runs, [], "_unoptimised")
        compute_jvml_file_size(plot_name, benchmark_name, "_unoptimised")

        generate_jvml_jar(benchmark_name, number_of_runs, ["-const-fp"], "_constant_propagation")
        compute_jvml_file_size(plot_name, benchmark_name, "_constant_propagation")

        generate_jvml_jar(benchmark_name, number_of_runs, ["-peep"], "_peephole")
        compute_jvml_file_size(plot_name, benchmark_name, "_peephole")

        generate_jvml_jar(benchmark_name, number_of_runs, ["-tco"], "_tco")
        compute_jvml_file_size(plot_name, benchmark_name, "_tco")

        generate_jvml_jar(benchmark_name, number_of_runs, ["-tmm", "-tco"], "_tail_mod_monoid")
        compute_jvml_file_size(plot_name, benchmark_name, "_tail_mod_monoid")

        generate_jvml_jar(benchmark_name, number_of_runs, ["-tmc", "-tco"], "_tail_mod_cons")
        compute_jvml_file_size(plot_name, benchmark_name, "_tail_mod_cons")

        generate_jvml_jar(benchmark_name, number_of_runs, ["-inline"], "_inline")
        compute_jvml_file_size(plot_name, benchmark_name, "_inline")

    #plot_individual_opts_code_size(plot_name, [b[0] for b in benchmark_details])

def compare_compilers_size():
    plot_name = "comparing_compilers_size"
    for benchmark_name, number_of_runs in benchmark_details:
        generate_jvml_jar(benchmark_name, number_of_runs, ["-opt-all"], "_opt")
        compute_jvml_file_size(plot_name, benchmark_name, "_opt")

        generate_jvml_jar(benchmark_name, number_of_runs, [], "")
        compute_jvml_file_size(plot_name, benchmark_name, "")

        generate_sml_executables(benchmark_name, number_of_runs)
        compute_sml_file_sizes(plot_name, benchmark_name)

    #plot_compiler_size(plot_name, [b[0] for b in benchmark_details])

def compare_varied_inlining_threshold():
    plot_name = "comparing_inlining_threshold_short_constfp"
    inlining_thresholds = [1, 3, 5, 7, 9, 11]
    for benchmark_name, number_of_runs in quick_benchmark_details:
        #["-peep", "-const-fp", "-tco", "-tmm", "-tmc", "-inline"]
        for threshold in inlining_thresholds:
            generate_jmh_executables(benchmark_name, number_of_runs, ["-const-fp", "-inline", "-inl-threshold", f"{threshold}"], f"_{threshold}")
            run_jvml_benchmarking(plot_name, benchmark_name, f"_{threshold}")

        generate_jmh_executables(benchmark_name, number_of_runs, ["-const-fp"], "_no_inlining")
        run_jvml_benchmarking(plot_name, benchmark_name, "_no_inlining")

    #plot_varied_inlining_threshold(plot_name, [b[0] for b in benchmark_details])

def compare_varied_inlining_threshold_size():
    plot_name = "comparing_inlining_threshold_size"
    inlining_thresholds = [1, 3, 5, 7, 9, 11]
    for benchmark_name, number_of_runs in benchmark_details:
        for threshold in inlining_thresholds:
            generate_jvml_jar(benchmark_name, number_of_runs, ["-opt-all", "-inl-threshold", f"{threshold}"], f"_{threshold}")
            compute_jvml_file_size(plot_name, benchmark_name, f"_{threshold}")

        generate_jvml_jar(benchmark_name, number_of_runs, ["-peep", "-const-fp", "-tco", "-tmm", "-tmc"], "_no_inlining")
        compute_jvml_file_size(plot_name, benchmark_name, "_no_inlining")

def plot_varied_inlining_threshold_size(plot_name, benchmark_names):
    benchmark_results = {benchmark_name : parse_result_json(plot_name, benchmark_name, "file size") for benchmark_name in benchmark_names}

    ordered_size_by_threshold = defaultdict(list)
    for benchmark_name in benchmark_results:
        for threshold in sorted(benchmark_results[benchmark_name]):
            if threshold != "jvml_no_inlining":
                ordered_size_by_threshold[threshold].append(benchmark_results[benchmark_name][threshold] / benchmark_results[benchmark_name]["jvml_no_inlining"])

    ordered_size_by_threshold = prettify_names(ordered_size_by_threshold)

    plt.style.use("seaborn-v0_8-deep")

    _, ax = plt.subplots()

    x = np.arange(len(benchmark_names))
    width = 0.15
    multiplier = 0

    from functools import cmp_to_key



    for threshold, relative_size in sorted(ordered_size_by_threshold.items(), key = cmp_to_key(lambda t1, t2: int(t1[0]) - int(t2[0]))):
        offset = width * multiplier
        ax.bar(x + offset, np.array(relative_size)-1, width, bottom=1.0, label=threshold, edgecolor='black',linewidth=0.5)
        multiplier += 1

    ax.set_xlabel("Benchmark")

    #ticks = np.arange(0.9, 1.22, 0.02)
    #ax.set_yticks(ticks)
    ax.set_ylabel("Code size relative to no inlining")
    ax.set_xticks(x + width * (len(x))/2, benchmark_names)
    ax.tick_params(axis='x', pad=10)

    ax.spines['bottom'].set_position(('data', 1))
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

    ax.legend(title="Inlining threshold")

    save_figure(plot_name)

def plot_compile_times(plot_name, benchmark_names):
    def parse_json(benchmark_name):
        with open(f"benchmarking/results/{plot_name}/{benchmark_name}.json", "r") as file:
            compile_times = json.loads(file.read())

        timez = list(compile_times)
        prev = {}
        for i in range(1, len(timez)):
            prev[timez[i]] = timez[i-1]

        times = {}
        for phase in compile_times:
            if phase != "startup":
                times[phase] = compile_times[phase] - compile_times[prev[phase]]

        return times


    benchmark_results = {benchmark_name : parse_json(benchmark_name) for benchmark_name in benchmark_names}
    ordered_benchmarks_by_phase = defaultdict(list)
    for benchmark_name in benchmark_results:
        for phase in benchmark_results[benchmark_name]:
            ordered_benchmarks_by_phase[phase].append(benchmark_results[benchmark_name][phase])

    plt.style.use("seaborn-v0_8-deep")

    ordered_benchmarks_by_phase = prettify_names(ordered_benchmarks_by_phase)

    _, ax = plt.subplots()
    bottom = np.zeros(len(ordered_benchmarks_by_phase["parsing"]))

    for phase, durations in ordered_benchmarks_by_phase.items():
        ax.bar(benchmark_names, durations, 0.5, label=phase, bottom=bottom, edgecolor='black',linewidth=0.5)
        bottom += np.array(durations)

    ax.set_title("Compile times by phase")
    ax.set_ylabel("Compile time (seconds)")
    ax.set_xlabel("Benchmark")
    ax.legend(loc="upper right")

    save_figure(plot_name)


def compute_compile_times():
    plot_name = "compile_times"

    create_result_directories_if_not_exist(plot_name)

    for benchmark_name, number_runs in benchmark_details:
        generate_jvml_jar(benchmark_name, number_runs, ["-opt-all", "-dump_debug", f"benchmarking/results/{plot_name}/{benchmark_name}.json"], "")

    plot_compile_times(plot_name, [b[0] for b in benchmark_details])



def compare_specialised_peepholes():
    plot_name = "comparing_individual_peepholes_no_jit"


    for (benchmark_name, flag) in [("box_unbox", "-peep-box"), ("store_load", "-peep-store-load"), ("goto_label", "-peep-goto-label")]:
        generate_jmh_executables(benchmark_name, 1, [flag], "_interpreted_opt")
        run_jvml_benchmarking(plot_name, benchmark_name, "_interpreted_opt", jvm_flags=["-XX:-UseCompiler"])

        generate_jmh_executables(benchmark_name, 1, [], "_interpreted_unoptimised")
        run_jvml_benchmarking(plot_name, benchmark_name, "_interpreted_unoptimised", jvm_flags=["-XX:-UseCompiler"])

        generate_jmh_executables(benchmark_name, 1, [flag], "_jit_opt")
        run_jvml_benchmarking(plot_name, benchmark_name, "_jit_opt", jvm_flags=[])

        generate_jmh_executables(benchmark_name, 1, [], "_jit_unoptimised")
        run_jvml_benchmarking(plot_name, benchmark_name, "_jit_unoptimised", jvm_flags=[])

def rerun_all_plots():
    benchmark_names = [b[0] for b in benchmark_details]
    #plot_comparing_compilers_time("comparing_compilers_time", benchmark_names)
    plot_individual_opts_time("individual_opt_time", benchmark_names)
    plot_tail_mod_cons_graph("tail_mod_cons", [1, 3, 6, 10, 33, 66, 100])
    plot_individual_opts_code_size("individual_opt_size", benchmark_names)
    plot_compiler_size("comparing_compilers_size", benchmark_names)
    plot_varied_inlining_threshold("comparing_inlining_threshold", benchmark_names)
    plot_varied_inlining_threshold_size("comparing_inlining_threshold_size", benchmark_names)
    plot_compile_times("compile_times", benchmark_names)
    #plot_specialised_peephole("comparing_individual_peepholes_no_jit", ["box_unbox", "store_load", "goto_label"])


if __name__ == "__main__":
    benchmark_names = [b[0] for b in benchmark_details]
    quick_benchmark_names = [b[0] for b in quick_benchmark_details]

    #compare_individual_opt_time()
    #generate_jmh_executables("quicksort", 10, ["-inline"], "_inline")
    #run_jvml_benchmarking("individual_opt_time_short", "quicksort", "_inline")
    compare_varied_inlining_threshold()
    #plot_individual_opts_time("individual_opt_time_short", quick_benchmark_names)
    #rerun_all_plots()
    plot_varied_inlining_threshold("comparing_inlining_threshold_short_constfp", quick_benchmark_names)


