# Your Role
You are a sophisticated programmer. 

# My Request
Please refactor my code. I often use Python, elisp, shell script, latex, html, css ,,, and so on.

# Rules
1. "[REFACTORED]" Tag
Add the tag "[REFACTORED]" at the end of the lines you revised as comments in the language, which should be estimated from the code itself.
  - For example, it must be # [REFACTORED] in shell / python scripts and %% [REFACTORED] in tex files.
  - When "[REFACTORED]" tags are already found in my input, please remove them. I need only the latest revision update.

1. Rule #1
...

2. Rule #2
...

# My Input
PLACEHOLDER

```
def load_markdown(lpath_md, style="plain_text"):
    import markdown
    import html2text
    
    # Load Markdown content from a file
    with open('example.md', 'r') as file:
        markdown_content = file.read()

    # Convert Markdown to HTML
    html_content = markdown.markdown(markdown_content)
    if style == "html":
        return html_content

    elif style == "plain_text":
        text_maker = html2text.HTML2Text()
        text_maker.ignore_links = True
        text_maker.bypass_tables = False
        plain_text = text_maker.handle(html_content)

        return plain_text
```        



### Rules Start ###
Add the tag "[REFACTORED]" at the end of the lines you revised as comments in the language.
For example, it must be # [REFACTORED] in shell / python scripts and %% [REFACTORED] in tex files.
When [REFACTORED] tags are already included, please remove them as I only need the current update.
Please avoid unnecessary comments as they will be disruptive.
Just return all the refactored code without any comments; I only need the code.

If your output will be over 100 lines, ask me whther it should be really presented. Then, if I say yes, please show me the entire code.

Docstring should follow the following style:

def ax_map_ticks(ax, src, tgt, axis="x"):
\"""
Maps source tick positions or labels to new target labels on a matplotlib Axes ...

Parameters:
- ax (matplotlib.axes.Axes): The Axes object to modify.
- src (list of str or numeric): Source positions (if numeric) or labels (if str) to map from.
When using string labels, ensure they match the current tick labels on the axis.
...

Returns:
- ax (matplotlib.axes.Axes): The modified Axes object with adjusted tick labels.

Examples:
--------
fig, ax = plt.subplots()
x = np.linspace(0, 2 * np.pi, 100)
y = np.sin(x)
ax.plot(x, y)  # Plot a sine wave
src = [0, np.pi/2, np.pi, 3*np.pi/2, 2*np.pi]  # Numeric src positions
tgt = ['0', 'π/2', 'π', '3π/2', '2π']  # Corresponding target labels
ax_map_ticks(ax, src, tgt, axis="x")  # Map src to tgt on the x-axis
plt.show()
\"""
### Rules End ###

Now, the code to refactor is as below:
