import os
import shutil
import subprocess
import csv
import matplotlib.pyplot as plt
import numpy as np
from collections import defaultdict
import json


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


    if not os.path.exists(f"benchmarking/results/{plot_name}/{benchmark_name}.json"):
        with open(f"benchmarking/results/{plot_name}/{benchmark_name}.json", "w") as file:
            file.write("{}")

    with open(f"benchmarking/results/{plot_name}/{benchmark_name}.json", "r") as existing_file:
        existing_data = json.loads(existing_file.read())

    for compiler in modified_json:
        existing_data[compiler] = modified_json[compiler]

    with open(f"benchmarking/results/{plot_name}/{benchmark_name}.json", "w") as existing_file:
        json.dump(existing_data, existing_file, indent=4, ensure_ascii=False)


'''
given that the executables exist in tmp, run each sml version

for now, just use hyperfine
'''
def run_sml_benchmarking(plot_name : str, benchmark_name : str, warmups=5, runs=10):
    if not os.path.exists("benchmarking/results"):
        os.mkdir("benchmarking/results")

    if not os.path.exists(f"benchmarking/results/{plot_name}"):
        os.mkdir(f"benchmarking/results/{plot_name}")

    subprocess.run(
        ["hyperfine",
         "--export-json", f"benchmarking/results/{benchmark_name}_tmp.json",
         "--warmup", f"{warmups}", "--min-runs", f"{runs}",
         f"benchmarking/out/{benchmark_name}.mlton",
         f"benchmarking/out/{benchmark_name}.mosml",
         f"benchmarking/out/{benchmark_name}.polyc"
    ])

    rewrite_hyperfile_out_json(plot_name, benchmark_name)

    os.remove(f"benchmarking/results/{benchmark_name}_tmp.json")


'''
Clear .java, .jar and .class files from previous benchmarking runs

Required we currently generate code belonging to the same package,
which might lead to conflicting names.
'''
def clear_java_directories() -> None:
    for file in os.listdir("benchmarking/jmh/lib"):
        os.remove("benchmarking/jmh/lib/" + file)

    for file in os.listdir("benchmarking/jmh/src/main/java/jvml/benchmark"):
        os.remove("benchmarking/jmh/src/main/java/jvml/benchmark/" + file)

    if os.path.exists("benchmarking/jmh/target"):
        shutil.rmtree("benchmarking/jmh/target")

ALL_OPTIMISATIONS : list[str] = ["-peep", "-const-fp", "-tco", "-tmm", "-tmc"]

def generate_jvml_executables(benchmark_name : str, number_runs : int, optimisation_flags : list[str], result_suffix : str) -> None:

    if os.path.exists("benchmarking/tmp"):
        shutil.rmtree("benchmarking/tmp")
    os.mkdir("benchmarking/tmp")

    clear_java_directories()

    with open(f"benchmarking/programs/{benchmark_name}.jvml", "r") as file:
        prog = file.read()

    jvml_program = prog + "\n" + f"val benchmark = doit {number_runs}"

    with open(f"benchmarking/tmp/{benchmark_name}.jvml", "w") as file:
        file.write(jvml_program)

    class_name = benchmark_name.capitalize() + result_suffix
    out_name = benchmark_name + result_suffix

    subprocess.run(["bash", "scripts/build_jar.sh",
                    "-f", f"benchmarking/tmp/{benchmark_name}.jvml",
                    "-o", class_name] + optimisation_flags
                    )
    if os.path.exists(f"benchmarking/jmh/lib/{benchmark_name}.jar"):
        subprocess.run(["rm", f"benchmarking/jmh/lib/{benchmark_name}.jar"])
    subprocess.run(["mv", f"{class_name}.jar", f"benchmarking/jmh/lib/{out_name}.jar"])


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
                .warmupTime(TimeValue.seconds(1))
                .measurementTime(TimeValue.seconds(1))
                .forks(1)
                .build();
        new Runner(opt).run();
    }}

    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
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
        config = file.read()

    config = config.split("</dependencies>")

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

    shutil.rmtree("benchmarking/tmp")

'''
rewrite jmh's output .json into our unified format
'''
def rewrite_jmh_out_json(plot_name : str, benchmark_name : str, result_suffix : str, result_file : str) -> None:
    out_name = benchmark_name + result_suffix
    with open(f"benchmarking/results/{out_name}_tmp.json", "r") as tmp_file:
        tmp_data = json.loads(tmp_file.read())


    compiler = "jvml" + result_suffix
    mean = tmp_data[0]["primaryMetric"]["score"] if len(tmp_data) > 0 else 0

    modified_json = {
        compiler: {
            "mean" : mean
        }
    }

    if not os.path.exists(f"benchmarking/results/{plot_name}/{result_file}.json"):
        with open(f"benchmarking/results/{plot_name}/{result_file}.json", "w") as file:
            file.write("{}")

    with open(f"benchmarking/results/{plot_name}/{result_file}.json", "r") as existing_file:
        existing_data = json.loads(existing_file.read())

    for compiler in modified_json:
        existing_data[compiler] = modified_json[compiler]

    with open(f"benchmarking/results/{plot_name}/{result_file}.json", "w") as existing_file:
        json.dump(existing_data, existing_file, indent=4, ensure_ascii=False)



'''
given that the executables exist in jmh/src/main/..., run each benchmark
'''
def run_jvml_benchmarking(plot_name : str, benchmark_name : str, result_suffix : str, result_file : str | None = None):

    if result_file is None:
        result_file = benchmark_name

    if not os.path.exists("benchmarking/results"):
        os.mkdir("benchmarking/results")

    if not os.path.exists(f"benchmarking/results/{plot_name}"):
        os.mkdir(f"benchmarking/results/{plot_name}")

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
        #"-Djmh.ignoreLock=true",
        "-classpath", f"/Users/sam/jvml/benchmarking/jmh/target/classes:/Users/sam/.m2/repository/org/openjdk/jmh/jmh-core/1.37/jmh-core-1.37.jar:/Users/sam/.m2/repository/net/sf/jopt-simple/jopt-simple/5.0.4/jopt-simple-5.0.4.jar:/Users/sam/.m2/repository/org/apache/commons/commons-math3/3.6.1/commons-math3-3.6.1.jar:/Users/jvml/.m2/repository/org/openjdk/jmh/jmh-generator-annprocess/1.37/jmh-generator-annprocess-1.37.jar:/Users/sam/jvml/benchmarking/jmh/lib/{out_name}.jar",
        f"jvml.benchmark.Benchmark{class_name}", "+TieredCompilation", "-Xmx8589934592",
    ])

    rewrite_jmh_out_json(plot_name, benchmark_name, result_suffix, result_file)

    os.remove(f"benchmarking/results/{out_name}_tmp.json")


def parse_result_csv(plot_name : str, benchmark_name : str) -> dict[str, float]:
    with open(f"benchmarking/results/{plot_name}/{benchmark_name}.json", "r") as file:
        benchmark_results = json.loads(file.read())

    time_by_key = {key : benchmark_results[key]["mean"] for key in benchmark_results}

    return time_by_key

'''
given a list of benchmarks that have been run (with results as csv files in the 'csv' directory)

produce a bar plot of performance
'''
def plot_peformance_graphs(plot_name : str, benchmark_names : list[str]) -> None:
    benchmark_results = {benchmark_name : parse_result_csv(plot_name, benchmark_name) for benchmark_name in benchmark_names}
    ordered_benchmarks_by_compiler = defaultdict(list)
    for benchmark_name in benchmark_results:
        for compiler in benchmark_results[benchmark_name]:
            ordered_benchmarks_by_compiler[compiler].append(benchmark_results[benchmark_name][compiler] / benchmark_results[benchmark_name]["mlton"])

    _, ax = plt.subplots()

    x = np.arange(len(benchmark_names))  # the label locations
    width = 0.15  # the width of the bars
    multiplier = 0


    for compiler, execution_time in ordered_benchmarks_by_compiler.items():
        offset = width * multiplier
        ax.bar(x + offset, execution_time, width, label=compiler)
        for i in range(len(execution_time)):
            if execution_time[i] == 0:
                ax.text((x+offset)[i], 1, f"{compiler} failed", rotation="vertical", size=6, ha="center")
        multiplier += 1

    ax.set_xlabel("Benchmark")
    ax.set_ylabel("Execution time relative to mlton")
    ax.set_xticks(x + width * (len(x)+1)/2, benchmark_names)
    ax.legend()

    plt.show()


def plot_individual_opts_graphs(plot_name : str, benchmark_names : list[str]) -> None:
    benchmark_results = {benchmark_name : parse_result_csv(plot_name, benchmark_name) for benchmark_name in benchmark_names}
    ordered_benchmarks_by_opt = defaultdict(list)
    for benchmark_name in benchmark_results:
        for optimisation in benchmark_results[benchmark_name]:
            if optimisation != "jvml_unoptimised":
                ordered_benchmarks_by_opt[optimisation].append(benchmark_results[benchmark_name][optimisation] / benchmark_results[benchmark_name]["jvml_unoptimised"])

    _, ax = plt.subplots()

    x = np.arange(len(benchmark_names))  # the label locations
    width = 0.15  # the width of the bars
    multiplier = 0

    for optimisation, relative_execution_time in ordered_benchmarks_by_opt.items():
        offset = width * multiplier
        ax.bar(x + offset, np.array(relative_execution_time)-1, width, bottom=1.0, label=optimisation)
        for i in range(len(relative_execution_time)):
            if relative_execution_time[i] == 0:
                ax.text((x+offset)[i], 1, f"jvml with only {optimisation} failed", rotation="vertical", size=6, ha="center")
        multiplier += 1

    ax.set_xlabel("Benchmark")
    ax.set_ylim([0.5, 1.5])
    ticks = np.arange(0.5, 1.5, 0.1)
    ax.set_yticks(ticks)
    ax.set_ylabel("Execution time relative to unoptimised jvml")
    ax.set_xticks(x + width * (len(x)+1)/2, benchmark_names)

    ax.spines['bottom'].set_position(('data', 1))
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)


    ax.legend()

    plt.show()

def plot_tail_mod_cons_bar_graph(plot_name : str, list_sizes : list[int]):
    benchmark_results = {list_size : parse_result_csv(plot_name, list_size) for list_size in list_sizes}
    ordered_benchmarks_by_size = defaultdict(list)
    for benchmark_name in benchmark_results:
        for map_type in benchmark_results[benchmark_name]:
            ordered_benchmarks_by_size[map_type].append(benchmark_results[benchmark_name][map_type] / benchmark_results[benchmark_name]["jvml_naive_map"])

    _, ax = plt.subplots()

    x = np.arange(len(list_sizes))  # the label locations
    width = 0.15  # the width of the bars
    multiplier = 0

    for map_type, relative_execution_time in ordered_benchmarks_by_size.items():
        offset = width * multiplier
        ax.bar(x + offset, relative_execution_time, width, label=map_type)
        for i in range(len(relative_execution_time)):
            if relative_execution_time[i] == 0:
                ax.text((x+offset)[i], 1, f"{map_type} failed", rotation="vertical", size=6, ha="center")
        multiplier += 1

    ax.set_xlabel("list size")
    ax.set_ylabel("Execution time relative to naive map")
    ax.set_xticks(x + width * (len(x)+1)/2, list_sizes)

    ax.legend()

    plt.show()

def plot_tail_mod_cons_graph(plot_name : str, list_sizes : list[int]) -> None:
    benchmark_results = {list_size : parse_result_csv(plot_name, list_size) for list_size in list_sizes}
    ordered_benchmarks_by_size = defaultdict(list)
    for benchmark_name in benchmark_results:
            for map_type in benchmark_results[benchmark_name]:
                ordered_benchmarks_by_size[map_type].append(benchmark_results[benchmark_name][map_type] / benchmark_results[benchmark_name]["jvml_map_rev"])


    _, ax = plt.subplots()
    for map_type, relative_execution_time in ordered_benchmarks_by_size.items():
        ax.plot(list_sizes, relative_execution_time, label=map_type)

    ax.set_xscale("log")

    ax.set_xlabel("log(list size)")
    ax.set_ylabel("execution time relative to map + rev")

    ax.legend()
    plt.show()





benchmark_details = [
    ("life", 1),
    ("tak", 1),
    ("mandlebrot", 1),
    ("quicksort", 100),
    ("brzozowski", 1000)
    ]

def compare_compiler_benchmark():
    plot_name = "comparing_compilers"
    for benchmark_name, number_of_runs in benchmark_details:
        generate_sml_executables(benchmark_name, number_of_runs)
        run_sml_benchmarking(plot_name, benchmark_name, warmups=1, runs=1) #lazy for now
        generate_jvml_executables(benchmark_name, number_of_runs, ALL_OPTIMISATIONS, "_opt")
        run_jvml_benchmarking(plot_name, benchmark_name, "_opt")
        generate_jvml_executables(benchmark_name, number_of_runs, [], "")
        run_jvml_benchmarking(plot_name, benchmark_name, "")

    plot_peformance_graphs(plot_name, [b[0] for b in benchmark_details])

def compare_individual_optimisations():
    plot_name = "individual_optimisations"
    for benchmark_name, number_of_runs in benchmark_details:

        generate_jvml_executables(benchmark_name, number_of_runs, [], "_unoptimised")
        run_jvml_benchmarking(plot_name, benchmark_name, "_unoptimised")

        generate_jvml_executables(benchmark_name, number_of_runs, ["-const-fp"], "_constant_propagation")
        run_jvml_benchmarking(plot_name, benchmark_name, "_constant_propagation")

        generate_jvml_executables(benchmark_name, number_of_runs, ["-peep"], "_peephole")
        run_jvml_benchmarking(plot_name, benchmark_name, "_peephole")

        generate_jvml_executables(benchmark_name, number_of_runs, ["-tco"], "_tco")
        run_jvml_benchmarking(plot_name, benchmark_name, "_tco")

        generate_jvml_executables(benchmark_name, number_of_runs, ["-tmm", "-tco"], "_tail_mod_monoid")
        run_jvml_benchmarking(plot_name, benchmark_name, "_tail_mod_monoid")

        generate_jvml_executables(benchmark_name, number_of_runs, ["-tmc", "-tco"], "_tail_mod_cons")
        run_jvml_benchmarking(plot_name, benchmark_name, "_tail_mod_cons")

    plot_individual_opts_graphs(plot_name, [b[0] for b in benchmark_details])

def compare_tail_mod_cons():
    plot_name = "tail_mod_cons"
    list_sizes = [1, 3, 6, 10, 33, 66, 100, 333, 666, 1000, 3333, 6666, 10_000, 33_333, 66_666, 100_000, 333_333, 666_666, 1_000_000]
    for list_size in list_sizes:
        generate_jvml_executables("map", list_size, ["-tmc", "-tco"], f"_map_tmc")
        run_jvml_benchmarking(plot_name, "map", f"_map_tmc", result_file=str(list_size))

        generate_jvml_executables("map_cps_defun", list_size, ["-tco"], f"_map_cps_defun")
        run_jvml_benchmarking(plot_name, "map_cps_defun", f"_map_cps_defun", result_file=str(list_size))

        generate_jvml_executables("map_rev", list_size, ["-tco"], f"_map_rev")
        run_jvml_benchmarking(plot_name, "map_rev", f"_map_rev", result_file=str(list_size))

        #generate_jvml_executables("map", list_size, ["-tco"], f"_naive_map")
        #run_jvml_benchmarking(plot_name, "map", f"_naive_map", result_file=str(list_size))

    plot_tail_mod_cons_graph(plot_name, list_sizes)


if __name__ == "__main__":
    #compare_compiler_benchmark()
    #compare_individual_optimisations()
    #plot_individual_opts_graphs("individual_optimisations", ["brzozowski", "tak", "quicksort"])
    #compare_tail_mod_cons()
    list_sizes = [1, 3, 6, 10, 33, 66, 100, 333, 666, 1000, 3333, 6666, 10_000, 33_333, 66_666, 100_000, 333_333, 666_666, 1_000_000]
    plot_tail_mod_cons_graph("tail_mod_cons", list_sizes)
