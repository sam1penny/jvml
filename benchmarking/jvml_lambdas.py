import utils


def generate_program(n : int) -> str:
    prog = "val apply_lambdas = fun dead -> ( "
    for i in range(n):
        prog += f"fun x{i} -> "
    prog += "() )"
    for i in range(n):
        prog += " ()"


    return prog


if __name__ == "__main__":
    utils.setup_jvml_benchmark(generate_program(1024), "lambdas")