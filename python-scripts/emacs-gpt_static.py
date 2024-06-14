import argparse
import json
import os
import time
import openai
from openai import OpenAI
import mngs

# PATHs
def load_templates():
    BASE_DIR = mngs.path.split(__file__)[0]
    TEMPLATE_DIR = BASE_DIR + "/templates/"
    TEMPLATE_PATHS = mngs.gen.natglob(TEMPLATE_DIR + "*")
    TEMPLATE_NAMES = ["".join(mngs.path.split(l)[1:]) for l in TEMPLATE_PATHS]

    TEMPLATES = {fname.split(".")[0]: mngs.io.load(lpath) for lpath, fname
                 in zip(TEMPLATE_PATHS, TEMPLATE_NAMES)
                 }
    return TEMPLATES

def run_gpt(
    api_key,
    engine,
    max_tokens,
    temperature,
    api_type,
    prompt_file,
    history_file,
    task_type,
    N_HISTORY=3,
):
    # Read the prompt
    with open(prompt_file, "r") as f:
        prompt = f.read()

    # Prepend the INSTRUCTION PROMPT
    TEMPLATES = load_templates()
    prompt = TEMPLATES.get(task_type, task_type) + prompt

    # SciWrite is run on gpt-4
    engine = {"SciWrite": "gpt-4"}.get(task_type, engine)

    # Load the history
    if api_type == "chat":
        if os.path.exists(history_file):
            with open(history_file, "r") as f:
                history = json.load(f)[-N_HISTORY:]
        else:
            history = []
        history.append({"role": "user", "content": prompt})
    else:
        history = None

    # Set the api key
    openai.api_key = api_key
    client = OpenAI()

    # Depending on the api_type use the appropriate API
    if api_type == "chat":
        while True:
            try:
                response = client.chat.completions.create(
                    model=engine,
                    messages=history[-N_HISTORY:],
                    max_tokens=int(max_tokens),
                    temperature=float(temperature),
                )
                break

            except Exception as e:
                print(e)
                return e

        # Update the history with assistant's message
        history.append(
            {
                "role": "assistant",
                "content": response.choices[0].message.content,
            }
        )

    else:
        response = client.chat.completions.create(
            engine=engine,
            prompt=prompt,
            max_tokens=int(max_tokens),
            temperature=float(temperature),
        )

    # Save the updated history
    if api_type == "chat":
        with open(history_file, "w") as f:
            json.dump(history, f)

    print()
    print("\n" + "=" * 60 + "\n")
    for response in history:
        role = response["role"]
        role = {"user": "YOU", "assistant": "GPT"}[role]
        content = (
            response["content"]
            .replace("Assistant: ", "")
            .replace("assistant: ", "")
            .replace("User: ", "")
            .replace("user: ", "")
        )
        if role == "YOU":
            content.replace("\n\n", "")

        print(role)
        print()
        print(content)
        print("\n" + "=" * 60 + "\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run OpenAI GPT.")
    parser.add_argument("api_key")
    parser.add_argument("engine")
    parser.add_argument("max_tokens")
    parser.add_argument("temperature")
    parser.add_argument("api_type")
    parser.add_argument("prompt_file")
    parser.add_argument("history_file")
    parser.add_argument("task_type")
    args = parser.parse_args()

    run_gpt(
        args.api_key,
        args.engine,
        args.max_tokens,
        args.temperature,
        args.api_type,
        args.prompt_file,
        args.history_file,
        args.task_type,
    )
