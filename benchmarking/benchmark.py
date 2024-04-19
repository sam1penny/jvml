import os
import shutil
import subprocess
import csv
import matplotlib.pyplot as plt
import numpy as np
from collections import defaultdict


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

def generate_jvml_executables(benchmark_name : str, number_runs : int, opt=True) -> None:

    if os.path.exists("benchmarking/tmp"):
        shutil.rmtree("benchmarking/tmp")
    os.mkdir("benchmarking/tmp")

    clear_java_directories()

    with open(f"benchmarking/programs/{benchmark_name}.jvml", "r") as file:
        prog = file.read()

    jvml_program = prog + "\n" + f"val benchmark = doit {number_runs}"

    with open(f"benchmarking/tmp/{benchmark_name}.jvml", "w") as file:
        file.write(jvml_program)

    class_name = benchmark_name.capitalize() + ("Opt" if opt else "")
    out_name = benchmark_name + ("_opt" if opt else "")

    optimisation_flags = ["-peep", "-const-fp", "-tco", "-tmm", "-tmc"] if opt else []

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
                .resultFormat(ResultFormatType.CSV)
                .result("benchmarking/results/{out_name}_jvml.csv")
                .forks(1)
                .build();
        new Runner(opt).run();
    }}

    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
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
given that the executables exist in tmp, run each sml version

for now, just use hyperfine
'''
def run_sml_benchmarking(benchmark_name : str, warmups=5, runs=10):
    if not os.path.exists("benchmarking/results"):
        os.mkdir("benchmarking/results")

    subprocess.run(
        ["hyperfine",
         "--export-csv", f"benchmarking/results/{benchmark_name}.csv",
         "--warmup", f"{warmups}", "--min-runs", f"{runs}",
         f"benchmarking/out/{benchmark_name}.mlton",
         f"benchmarking/out/{benchmark_name}.mosml",
         f"benchmarking/out/{benchmark_name}.polyc"
    ])


'''
given that the executables exist in jmh/src/main/..., run each benchmark
'''
def run_jvml_benchmarking(benchmark_name : str, opt = True):

    class_name = benchmark_name.capitalize() + ("Opt" if opt else "")
    out_name = benchmark_name + ("_opt" if opt else "")


    if os.path.exists(f"benchmarking/results/{out_name}_jvml.csv"):
        os.remove(f"benchmarking/results/{out_name}_jvml.csv")

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

    # move jmh formatted csv to unified format


    with open(f"benchmarking/results/{out_name}_jvml.csv", "r") as file:
        reader = csv.reader(file)
        next(reader)
        data = list(reader)

    with open(f"benchmarking/results/{out_name}_jvml.csv", "w") as file:
        file.write("command,mean,stddev,median,user,system,min,max\n")
        for row in data:
            opt_type = "optimised" if opt else "unoptimised"
            file.write(",".join([f"_.jvml ({opt_type})", row[4], "0", "0", "0", "0", "0", "0"]) + "\n")




def parse_result_csv(benchmark_name : str) -> dict[str, float]:
    with open(f"benchmarking/results/{benchmark_name}.csv", "r") as file:
        reader = csv.reader(file)
        next(reader)
        data = list(reader)

    time_by_compiler = {d[0].split(".")[1] : float(d[1]) for d in data}

    with open(f"benchmarking/results/{benchmark_name}_jvml.csv", "r") as file:
        reader = csv.reader(file)
        next(reader)
        data = list(reader)

    if len(data) == 0:
        time_by_compiler["jvml (unoptimised)"] = 0
    else:
        time_by_compiler[data[0][0].split(".")[1]] = float(data[0][1]) / 1000 # currently in milliseconds

    with open(f"benchmarking/results/{benchmark_name}_opt_jvml.csv", "r") as file:
        reader = csv.reader(file)
        next(reader)
        data = list(reader)

    if len(data) == 0:
        time_by_compiler["jvml (unoptimised)"] = 0
    else:
        time_by_compiler[data[0][0].split(".")[1]] = float(data[0][1]) / 1000 # currently in milliseconds


    return time_by_compiler

'''
given a list of benchmarks that have been run (with results as csv files in the 'csv' directory)

produce a bar plot of performance
'''
def plot_peformance_graphs(benchmark_names : list[str]) -> None:
    benchmark_results = {benchmark_name : parse_result_csv(benchmark_name) for benchmark_name in benchmark_names}
    print(benchmark_results)
    ordered_benchmarks_by_compiler = defaultdict(list)
    for benchmark_name in benchmark_results:
        for compiler in benchmark_results[benchmark_name]:
            ordered_benchmarks_by_compiler[compiler].append(benchmark_results[benchmark_name][compiler] / benchmark_results[benchmark_name]["mlton"])

    _, ax = plt.subplots()

    x = np.arange(len(benchmark_names))  # the label locations
    width = 0.15  # the width of the bars
    multiplier = 0


    for compiler, execution_time in ordered_benchmarks_by_compiler.items():
        print(execution_time)
        offset = width * multiplier
        rects = ax.bar(x + offset, execution_time, width, label=compiler)
        for i in range(len(execution_time)):
            if execution_time[i] == 0:
                ax.text((x+offset)[i], 1, f"{compiler} failed", rotation="vertical", size=6, ha="center")
        multiplier += 1

    ax.set_xlabel("Benchmark")
    ax.set_ylabel("Execution time relative to mlton")
    ax.set_xticks(x + width, benchmark_names)
    ax.legend()

    plt.show()


benchmark_details = [
    #("life", 1),
    #("tak", 1),
    #("mandlebrot", 1),
    ("quicksort", 100),
    #("brzozowski", 1000)
    ]


if __name__ == "__main__":
    for benchmark_name, number_of_runs in benchmark_details:
    #    generate_sml_executables(benchmark_name, number_of_runs)

    #   run_sml_benchmarking(benchmark_name, warmups=1, runs=1) #lazy for now
        generate_jvml_executables(benchmark_name, number_of_runs, opt=True)
        run_jvml_benchmarking(benchmark_name, opt=True)
        generate_jvml_executables(benchmark_name, number_of_runs, opt=False)
        run_jvml_benchmarking(benchmark_name, opt=False)

    plot_peformance_graphs([b[0] for b in benchmark_details])