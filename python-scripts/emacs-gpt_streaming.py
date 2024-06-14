import argparse
import json
import os
import time
import openai
from openai import OpenAI
import mngs
from mngs.path import split as mngs_path_split
from mngs.gen import natglob as mngs_gen_natglob
from mngs.io import load as mngs_io_load
from mngs.ai import GenAI as mngs_ai_GenAI

import sys

# PATHs
# __file__ = "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-gpt/python-scripts/emacs-gpt_streaming.py"


def load_templates():
    # TEMPLATE_DIR = mngs.path.split(__file__)[0] + "../templates/"
    # TEMPLATE_PATHS = mngs.gen.natglob(TEMPLATE_DIR + "*")
    TEMPLATE_DIR = mngs_path_split(__file__)[0] + "../templates/"
    TEMPLATE_PATHS = mngs_gen_natglob(TEMPLATE_DIR + "*")

    TEMPLATE_NAMES = ["".join(mngs_path_split(l)[1:]) for l in TEMPLATE_PATHS]

    TEMPLATES = {}
    for lpath, fname in zip(TEMPLATE_PATHS, TEMPLATE_NAMES):
        task_type = fname.split(".")[0]
        prompt = mngs_io_load(lpath, verbose=False)
        TEMPLATES[task_type] = prompt

    return TEMPLATES


def print_splitter(role):
    print(f"\n{'='* 60}\n\n{role}\n")


def concat_lines(lpath):
    prompt = [f"{line}\n" for line in mngs_io_load(lpath)]
    return "".join(prompt)


def run_gpt(
    api_key,
    engine,
    max_tokens,
    temperature,
    # prompt_file,
    history_file,
    task_type,
    n_history,
    prompt,
):

    # Parameters
    TEMPLATES = load_templates()
    # chat_history_all = mngs_io_load(history_file.replace(".json", "_all.json"))
    chat_history = mngs_io_load(history_file)

    # Read the prompt
    # prompt = concat_lines(prompt_file)

    # Embeds the prompt into template
    template = TEMPLATES.get(task_type, "PLACEHOLDER")
    ai_prompt = template.replace("PLACEHOLDER", prompt)

    # Your Prompt
    print_splitter("YOU")
    print(prompt)

    # GPT
    print_splitter("GPT")
    model = mngs_ai_GenAI(model=engine, stream=True, n_keep=10)
    # model.chat_history
    model(ai_prompt)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="")
    parser.add_argument(
        "--api_key",
        type=str,
        default=os.getenv("OPENAI_API_KEY"),
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--engine",
        type=str,
        default="gpt-3.5-turbo",
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--max_tokens",
        type=int,
        default=4096,
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--temperature",
        type=int,
        default=0,
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--prompt_file",
        type=str,
        default=None,
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--history_file",
        type=str,
        default=mngs.path.split(__file__)[0] + "../history.json",
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--n_history",
        type=int,
        default=5,
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--task_type",
        type=str,
        default="",
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--prompt",
        type=str,
        default="",
        help="(default: %(default)s)",
    )
    
    args = parser.parse_args()
    # mngs.gen.print_block(args, c="yellow")

    run_gpt(
        api_key=args.api_key,
        engine=args.engine,
        max_tokens=args.max_tokens,
        temperature=args.temperature,
        # prompt_file=args.prompt_file,
        history_file=args.history_file,
        task_type=args.task_type,
        n_history=args.n_history,
        prompt=args.prompt,        
    )
