import json
import os


def print_improvements_of_all_opts():
    for filename in os.listdir("benchmarking/results/comparing_compilers_time"):
        with open(f"benchmarking/results/comparing_compilers_time/{filename}", "r") as file:
            data = json.loads(file.read())

        improvement = data["jvml_opt"]["primaryMetric"]["score"] / data["jvml"]["primaryMetric"]["score"] * 100

        print(f"improvement on {filename} = {100-improvement}%")


if __name__ == "__main__":
    print_improvements_of_all_opts()
