import argparse
import openai
import json
import os
import time

INSTRUCTION_DEBUG = """
    You are a sophisticated programmer. Please debug my code below, aligning with the following rules.
     
    ### Rules Start ###
    Add the tag "[REVISED]" at the end of the lines you revised as comments in the language.
    For example, it must be # [REVISED] in shell / python scripts and %% [REVISED] in tex files.
    When [REVISED] tags are already included, please remove them as I only need the current update.
    Please avoid unnecessary comments as they will be disruptive.
    Just return all the debugged code without any comments; I only need the code.
    ### Rules End ###
     
    Now, the code to debug is as below:
    """

INSTRUCTION_REFACTOR = """
    You are a sophisticated programmer. Please refactor my code below, aligning with the following rules.
     
    ### Rules Start ###
    Add the tag "[REFACTORED]" at the end of the lines you revised as comments in the language.
    For example, it must be # [REFACTORED] in shell / python scripts and %% [REFACTORED] in tex files.
    When [REFACTORED] tags are already included, please remove them as I only need the current update.
    Please avoid unnecessary comments as they will be disruptive.
    Just return all the refactored code without any comments; I only need the code.
    ### Rules End ###
     
    Now, the code to refactor is as below:
    """

INSTRUCTION_SCIWRITE = \
    """
    You are an esteemed professor in the scientific field, based in the United States.
    The subsequent passages originate from a student whose first language is not English.
    Please proofread them with following the rules below.
    
    - Correct the English without including messages or comments.
    - Retain the original syntax as much as possible while conforming to scholarly language.
    - Do not modify linguistically correct sections.
    - Minimize revisions to avoid endless paraphrasing.
    - Exclude comments beyond the revised text.
    - Preserve LaTeX commands as needed.
    - Avoid unnecessary adjectives not suitable for scientific writing, such as "somewhat", "in-depth", and "various".
    - For figures and tables, please use tags like Figure~\ref{fig:01}A or Table~\ref{tab:01}.
    
    - Highlight parts that may require the author's manual review due to ambiguity using CHECKME tags as follows: [CHECKME>] This is an ambiguous sentence. [<CHECKME ENDS].
    - If [FIXME ->][<- FIXME] tags are present, please revise only the enclosed area; otherwise, please revise the entire text.
    - When using --- (emdash), please add spaces on either side.
    - Terminology should be consistent throughout the manuscript.

    - Titles should follow the capitalization rules for titles like this: Local Field Potentials, Multiunit Activity, and Neural Trajectories in the Hippocampus during a Modified Sternberg Task. Please note that prepositions are written in lower letters. When a singular form without a preposition such as (a, an, the) is appropriate, it is preferred.

    - Titles of figures and tables should be the nown form

    - The legend of figures and tables should use noun forms as much as possible.
    
    Now, the original manuscript to be revised is as follows:
    """

INSTRUCTION_CORRECT = \
    """
    Since I am not a native English speaker, please correct my English.
    Do not include any messages nor comments as I can not discern them from your revision.
    Now, my sentences are below:
    """

INSTRUCTION_PROMPTS = {
    "Fix": INSTRUCTION_DEBUG,
    "SciWrite": INSTRUCTION_SCIWRITE,
    "Correct": INSTRUCTION_CORRECT,
    "Refactor": INSTRUCTION_REFACTOR,        
}


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
    prompt = INSTRUCTION_PROMPTS.get(task_type, task_type) + prompt

    # SciWrite is run on gpt-4
    engine = {"SciWrite": "gpt-4"
              }.get(task_type, engine)
    
    # if task_type == "SciWrite":
    #     engine = "gpt-4"

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

    # Depending on the api_type use the appropriate API
    if api_type == "chat":
        while True:
            try:
                response = openai.ChatCompletion.create(
                    model=engine,
                    messages=history[-N_HISTORY:],
                    max_tokens=int(max_tokens),
                    temperature=float(temperature),
                )
                break
            except openai.error.InvalidRequestError as e:
                if "context length is" in str(e):
                    # Reduce the history and try again by excluding the first message
                    history = history[1:]
                    print(e)
                else:
                    raise e

            except openai.error.RateLimitError as e:
                print(e)
                time.sleep(3)

        # Update the history with assistant's message
        history.append(
            {
                "role": "assistant",
                "content": response["choices"][0]["message"]["content"],
            }
        )
    else:
        response = openai.Completion.create(
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