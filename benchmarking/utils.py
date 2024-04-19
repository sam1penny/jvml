import os

def setup_jvml_benchmark(program : str, benchmark_name : str) -> None:
    with open(f"benchmarking/programs/{benchmark_name}.jvml", "w") as file:
        file.write(program)

    os.system(f"bash scripts/build_jar.sh -f benchmarking/programs/{benchmark_name}.jvml")
    os.system(f"rm benchmarking/jmh/lib/{benchmark_name}.jar")
    os.system(f"mv {benchmark_name}.jar benchmarking/jmh/lib/{benchmark_name}.jar")