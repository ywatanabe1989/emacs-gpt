import argparse
import openai
import json
import os


def run_gpt(
    api_key, engine, max_tokens, temperature, api_type, prompt_file, history_file
):
    # Read the prompt
    with open(prompt_file, "r") as f:
        prompt = f.read()

    # Load the history
    if api_type == "chat":
        # Load the history if it exists, otherwise create an empty list
        if os.path.exists(history_file):
            with open(history_file, "r") as f:
                history = json.load(f)
        else:
            history = []

        history.append({"role": "user", "content": prompt})

    else:
        history = None

    # Set the api key
    openai.api_key = api_key

    # Depending on the api_type use the appropriate API
    if api_type == "chat":
        response = openai.ChatCompletion.create(
            model=engine,
            messages=history[-1000:],  # Adjust this value as per your requirement
            max_tokens=int(max_tokens),
            temperature=float(temperature),
        )
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

    history.reverse()

    # print("---")
    print()
    for ii in range(len(history) // 2):
        gpt_message = (
            history[2 * ii]["content"]
            .replace("Assistant: ", "")
            .replace("assistant: ", "")
            .replace("\n\n", "")
        )

        your_message = (
            history[2 * ii + 1]["content"]
            .replace("User: ", "")
            .replace("user: ", "")
            .replace("\n\n", "")
        )

        if ii != 0:
            print("YOU: " + your_message)
            # print()
        print()
        print("GPT: " + gpt_message)
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
