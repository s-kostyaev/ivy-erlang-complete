# ivy-erlang-complete


[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/ivy-erlang-complete-badge.svg)](https://melpa.org/#/ivy-erlang-complete)

`ivy-erlang-complete` is context sensitive completion for erlang
without connecting to erlang nodes.

## Installation

You can now install package `ivy-erlang-complete` from
[MELPA](https://melpa.org/#/getting-started). Just `M-x`
`package-install`<kbd>Enter</kbd> `ivy-erlang-complete` <kbd>Enter</kbd>.

### System packages
 * findutils
 * coreutils
 * sed
 * grep
 * the_silver_searcher (for finding definition, references and specs)
 
## Basic setup

For start using you need add to `init.el` something like this:

``` emacs-lisp
(require 'ivy-erlang-complete)
(add-hook 'erlang-mode-hook #'ivy-erlang-complete-init)
;; automatic update completion data after save
(add-hook 'after-save-hook '#ivy-erlang-complete-reparse)
```

### Advanced setup

But for better experience you also can add another packages for another
features:

* `rebar` for building & testing
* `flycheck` for on the fly error checking
* `wrangler` for smart refactoring

This is my emacs config for erlang developement:

``` emacs-lisp
;;;; Erlang
(setq flycheck-erlang-include-path '("../include" "../deps"))

(defun fix-erlang-project-includes (project-root)
  "Find erlang include paths for PROJECT-ROOT with project deps."
  (setq-local flycheck-erlang-include-path
              (append
               (s-split
                "\n"
                (shell-command-to-string
                 (concat "find "
                         project-root
                         "/*"
                         " -type d -name include"))
                t)
               (list project-root
                     (concat project-root "/include")
                     (concat project-root "/deps")
                     default-directory
                     (concat
                      (locate-dominating-file
                       default-directory
                       "src") "include")
                     (concat
                      (locate-dominating-file
                       default-directory
                       "src") "deps")))))

(defun fix-erlang-project-code-path (project-root)
  "Find erlang include paths for PROJECT-ROOT with project deps."
  (let ((code-path
           (split-string (shell-command-to-string
                        (concat "find " project-root " -type d -name ebin")))
         ))
    (setq-local flycheck-erlang-library-path code-path)))

(require 'ivy-erlang-complete)
(defun my-erlang-hook ()
  "Setup for erlang."
  (let ((project-root (ivy-erlang-complete-autosetup-project-root)))
      (fix-erlang-project-code-path project-root)
      (fix-erlang-project-includes project-root))
  (ivy-erlang-complete-init))
(add-hook 'erlang-mode-hook #'my-erlang-hook)
(add-hook 'after-save-hook #'ivy-erlang-complete-reparse)

;;; wrangler
;; install https://github.com/RefactoringTools/wrangler/blob/master/INSTALL
;; before usage
(add-to-list 'load-path "/usr/lib/erlang/lib/wrangler-1.2.0/elisp")
(require 'wrangler)
; Some code inspection functionalities of Wrangler generate .dot
; files, which can be compiled and previewed in Emacs if the
; Graphviz-dot mode for Emacs is enabled.
(load-library "graphviz-dot-mode")
```

## Current state

For now project is useful but can contain some bugs. There are context
sensitive completions for:
* local variables
* local functions
* module names
* module functions
* record names
* record fields
* macros names
* guards

Also you can find:
* definition of:
  * functions
  * macros
  * records
* references of:
  * functions
  * records
  * macros
* specs for:
  * functions
  * callbacks from used behaviours

And see documentation for functions from standart library in your browser.

![completions](https://github.com/s-kostyaev/ivy-erlang-complete/raw/master/completion.gif)
![go to definition references](https://github.com/s-kostyaev/ivy-erlang-complete/raw/master/gotodefref.gif)
![find spec](https://github.com/s-kostyaev/ivy-erlang-complete/raw/master/findspec.gif)

## Why not `distel`?

`distel` is great. I think `distel` much better then this package. But
I can't use it on my job for erlang developement. Only licensed nodes
can connect to cluster, distel nodes can not. I was trying to use esense
instead. But esense project is now dead and nobody fix crashes in
it. So I start this project.

## Why not `edts`?

`edts` use the same way for comunication with erlang as `distel`. I
don't use compiled beams for that - only parse source files. So `edts`
and `distel` can be more accurate in some cases. But I work on
improvements of this package. Also I don't like project configuration process
at all. So this package no need it - it work out of the box (if you
use hooks from advanced setup).

## Contributing

Patches are welcome :)
