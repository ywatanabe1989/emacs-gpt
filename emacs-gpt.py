import argparse
import openai
import json
import os
import time

ADDITIONAL_PROMPT = """
##########################
GEENRAL INSTRUCTION STARTS
##########################
Now, I am using you as (No. 1) a question answering tool, (No. 2) a debugging tool for programming, or (No. 3) a revision tool for scientific manuscripts. Your response must align with the following rules.

[Rules for No.1: Question Answering]
Rule 1-1: When question is included in the input text, please respond it with the PRE method: Point, Reason, and Example in about three sentences in total with bullet points like this:
- Point: (key message here)
- Reason: (the primary reason here)
- Example: (an example here)

[Rules for No.2: Debugging]
1: When code is input, include your revised script if necessary.
2: When [REVISED] tags are already included, please remove them as they are not needed any more.
3: Highlight the issue fixing lines using "[REVISED]" tag as tailing comments.

[Rules for No.3: Scientific Writer]
1: If revision for scieintific writing seems required, please use this prompt for you. "You are an esteemed professor in the scientific field, based in the United States. The subsequent passages originate from a student whose first language is not English. Please proofread these sentences in a manner that retains their original syntax as much as possible, yet conforms to the language style typical of a scholarly article in biology. Please do not modify any sections that do not pose linguistic challenges. Please keep the original sentences as much as possible and minimize your revisions because if you paraphrase the expressions all the time, this revision process will never end. I don't need any comments other than the revised text. The source sentences are as follows:"

[General Rules]
1: You shold avoid addiing unnecessary comments as I will be distructed.
########################
GEENRAL INSTRUCTION ENDS
########################
"""


def run_gpt(
    api_key,
    engine,
    max_tokens,
    temperature,
    api_type,
    prompt_file,
    history_file,
    N_HISTORY=3,
):
    # Read the prompt
    with open(prompt_file, "r") as f:
        prompt = f.read()

    # Prepend the ADDITIONAL_PROMPT to the user's prompt
    prompt = ADDITIONAL_PROMPT + prompt

    # Load the history
    N_MAX_HISTORY = 50
    if api_type == "chat":
        if os.path.exists(history_file):
            with open(history_file, "r") as f:
                history = json.load(f)
                # Truncate history to keep up to 50 past interactions [REVISED]
                history = history[-N_MAX_HISTORY:]
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
    args = parser.parse_args()

    run_gpt(
        args.api_key,
        args.engine,
        args.max_tokens,
        args.temperature,
        args.api_type,
        args.prompt_file,
        args.history_file,
    )
