import json
import itertools
import scipy.stats
import math
import numpy as np

with open("benchmarking/results/comparing_compilers_time/short_life.json") as file:
    data = json.loads(file.read())


def jmh_get_all_samples(data, key):
    hist = data[key]["primaryMetric"]["rawDataHistogram"]
    all_samples = []
    for i in range(len(hist)):
        for j in range(len(hist[i])):
            for k in range(len(hist[i][j])):
                for _ in range(hist[i][j][k][1]):
                    all_samples.append(hist[i][j][k][0])

    return np.array(all_samples)

def sample_mean_error_at(all_samples, confidence : float) -> float:
    N = len(all_samples)
    tdist = scipy.stats.t(N - 1)
    a = tdist.ppf(1 - (1 - confidence) / 2)
    return a * np.std(all_samples) / math.sqrt(N)


def jmh_extract_mean_error(data, key, confidence) -> float:
    all_samples = jmh_get_all_samples(data, key)
    return sample_mean_error_at(all_samples, confidence)

def hyperfine_get_all_samples(data, key):
    return np.array(data[key]["times"])

def hyperfine_extract_mean_error(data, key, confidence) -> float:
    return sample_mean_error_at(hyperfine_get_all_samples(data, key), confidence)

