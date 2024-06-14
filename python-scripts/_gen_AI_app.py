#!./env/bin/python3
# -*- coding: utf-8 -*-
# Time-stamp: "2024-06-11 04:14:42 (ywatanabe)"
# /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-gpt/gen_AI_app.py


"""
This script does XYZ.
"""


"""
Imports
"""
# import os
# import re
# import sys

# import matplotlib
# import matplotlib.pyplot as plt
# import mngs
# import seaborn as sns

# mngs.gen.reload(mngs)
# import warnings
# from glob import glob
# from pprint import pprint

# import numpy as np
# import pandas as pd
# import torch
# import torch.nn as nn
# import torch.nn.functional as F
# import xarray as xr
# from icecream import ic
# from natsort import natsorted
# from tqdm import tqdm

# sys.path = ["."] + sys.path
# from scripts import utils, load

"""
Warnings
"""
# warnings.simplefilter("ignore", UserWarning)


"""
Config
"""
# CONFIG = mngs.gen.load_configs()


"""
Functions & Classes
"""
from flask import Flask, Response, jsonify, request, stream_with_context

# from your_model_file import main  # Import the main function from your script
from mngs.ai import GenAI


def main(
    model="gpt-3.5-turbo",
    stream=False,
    prompt="Hi, please tell me about the hippocampus",
    seed=None,
    temperature=1.0,
):

    m = GenAI(model, stream=stream, seed=seed, temperature=temperature)
    out = m(prompt)

    return out


app = Flask(__name__)


# @app.route("/predict", methods=["POST"])
# def predict():
#     data = request.json

#     def generate():
#         for output in main(
#             model=data.get("model", "gpt-3.5-turbo"),
#             stream=True,
#             prompt=data.get("prompt", ""),
#             seed=data.get("seed"),
#             temperature=data.get("temperature", 1.0),
#         ):
#             yield output + "\n"

#     return Response(stream_with_context(generate()), mimetype="text/plain")


@app.route("/predict", methods=["POST"])
def predict():
    data = request.json

    def generate():
        # Open the file in append mode
        with open("/tmp/buffering_file.txt", "a") as file:
            for output in main(
                model=data.get("model", "gpt-3.5-turbo"),
                stream=True,
                prompt=data.get("prompt", ""),
                seed=data.get("seed"),
                temperature=data.get("temperature", 1.0),
            ):
                # Write output to file and yield it
                output_with_newline = output + "\n"
                file.write(output_with_newline)
                file.flush()  # Ensure it's written to disk immediately
                yield output_with_newline

    return Response(stream_with_context(generate()), mimetype="text/plain")


if __name__ == "__main__":
    # app.run(debug=True, port=5000)
    app.run(debug=True, port=5000)


# def main():
#     pass


# if __name__ == "__main__":
#     # # Argument Parser
#     # import argparse
#     # parser = argparse.ArgumentParser(description='')
#     # parser.add_argument('--var', '-v', type=int, default=1, help='')
#     # parser.add_argument('--flag', '-f', action='store_true', default=False, help='')
#     # args = parser.parse_args()

#     # Main
#     CONFIG, sys.stdout, sys.stderr, plt, CC = mngs.gen.start(
#         sys, plt, verbose=False
#     )
#     main()
#     mngs.gen.close(CONFIG, verbose=False, notify=False)

# EOF
