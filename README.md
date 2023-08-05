# Installation
```
$ git clone git@github.com:ywatanabe1989/emacs-gpt.git ~/.emacs.d/lisp/emacs-gpt
```

# Demo
[![Watch the video](doc/screenshot.png)](doc/demo.mp4)

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
