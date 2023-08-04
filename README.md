# Installation
```
$ git clone git@github.com:ywatanabe1989/emacs-gpt.git ~/.emacs.d/lisp/emacs-gpt
```

# Emacs config
``` elisp
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-gpt")

(require 'emacs-gpt)
(setq gpt-openai-key "sk-***AN")
(setq gpt-openai-engine "gpt-4")
(setq gpt-script-path "~/.emacs.d/lisp/emacs-gpt/emacs-gpt.py")
(setq gpt-openai-max-tokens "2000")
(setq gpt-openai-temperature "0")

(define-key global-map (kbd "M-C-g") 'gpt-on-region)
(define-key global-map (kbd "M-S-C-g") 'gpt-clear-history)
```

# Example
```
Another function named 'gpt-clear-history (M-S-C-g) clears the chat history and quit the *GPT* buffer.

GPT: Yes, you're correct. The 'gpt-clear-history' function (invoked with the key combination Meta-Shift-Control-g) is used to clear the chat history in the *GPT* buffer. This can be useful when you want to start a new conversation or if the buffer becomes too cluttered. After the history is cleared, the *GPT* buffer is closed. If you have any more questions or need further clarification, feel free to ask!

============================================================

YOU: User can input text by selecting a region and call the elisp function 'gpt-on-region (M-C-g).

GPT: That's correct! Emacs is a highly customizable text editor and you can use it to interact with ChatGPT. By selecting a region and calling the 'gpt-on-region' function (with the key combination Meta-Control-g), you can send the selected text to ChatGPT. The response from ChatGPT will then be inserted into your buffer. If you have any questions or need help with this process, feel free to ask!

============================================================

YOU: I am using ChatGPT from Emacs.

GPT: That's great! How can I assist you with your ChatGPT on Emacs?

============================================================

YOU: Hello.

GPT: Hello! How can I assist you today?

============================================================
```
