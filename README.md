# ivy-erlang-complete


[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/ivy-erlang-complete-badge.svg)](https://melpa.org/#/ivy-erlang-complete)

`ivy-erlang-complete` is context sensitive completion for erlang
without connecting to erlang nodes.

## Installation

You can now install package `ivy-erlang-complete` from
[MELPA](https://melpa.org/#/getting-started). Just `M-x`
`package-install`<kbd>Enter</kbd> `ivy-erlang-complete` <kbd>Enter</kbd>.

Or just put `ivy-erlang-complete.el` to load path. You also need install
dependencies:
### Emacs packages (can be installed from [MELPA](https://melpa.org/))
 * `ivy`
 * `s.el`
 * `dash.el`
 
If you install this package from [MELPA](https://melpa.org/) all nedded
emacs packages will be installed automatically.

### System packages
 * findutils
 * coreutils
 * sed
 * grep
 
## Basic setup

For start using you need add to `init.el` something like this:

``` emacs-lisp
(require 'ivy-erlang-complete)
(add-hook 'erlang-mode-hook '(define-key erlang-mode-map (kbd "C-:")
                               'ivy-erlang-complete))
;; automatic update completion data after save
(add-hook 'after-save-hook 'ivy-erlang-complete-reparse)
```

### Advanced setup

But for better experience you also can add another packages for another
features:

* `rebar` for building & testing
* `flycheck` for on the fly error checking
* `eopengrok` for code navigation

This is my emacs config for erlang developement:

``` emacs-lisp
;;;; Erlang
(need-package 'erlang)
(require 'rebar)
(add-hook 'erlang-mode-hook 'rebar-mode)

(setq flycheck-erlang-include-path '("../include" "../deps"))

(defun fix-erlang-project-includes ()
  "Find erlang include paths for selected directory with project deps."
  (interactive)
  (let
      ((dir
        (expand-file-name (read-directory-name
                           "Select project directory:" default-directory))))
    (setq flycheck-erlang-include-path (append
                                        (s-split
                                         "\n"
                                         (shell-command-to-string
                                          (concat "find "
                                                  dir
                                                  "/*"
                                                  " -type d -name include"))
                                         t)
                                        (list dir
                                              (concat dir "/include")
                                              (concat dir "/deps")
                                              default-directory
                                              (concat
                                               (locate-dominating-file
                                                default-directory
                                                "src") "include")
                                              (concat
                                               (locate-dominating-file
                                                default-directory
                                                "src") "deps"))))))

(defun fix-erlang-project-code-path ()
  "Find erlang include paths for selected directory with project deps."
  (interactive)
  (let ((code-path
         (let
             ((dir
               (expand-file-name (read-directory-name
                                  "Select project directory:" default-directory))))
           (split-string (shell-command-to-string
                        (concat "find " dir " -type d -name ebin"))))
         ))
    (setq flycheck-erlang-library-path code-path)))
(require 'ivy-erlang-complete)
(add-hook 'erlang-mode-hook
          (lambda ()
             (define-key erlang-mode-map (kbd "C-:")
               'ivy-erlang-complete)
             (define-key erlang-mode-map (kbd "C-c C-h")
               'ivy-erlang-complete-show-doc-at-point)
             (define-key erlang-mode-map (kbd "C-c C-e")
               (lambda ()
                 (interactive)
                 (eopengrok-make-index-with-enable-projects
                  (ivy-erlang-complete-set-project-root))))
             (define-key erlang-mode-map (kbd "C-c C-d")
               (lambda () (interactive)
                 (if (ivy-erlang-complete-record-at-point)
                     (eopengrok-find-text
                      (concat "\""
                              (s-replace "#" "record("
                                         (ivy-erlang-complete-thing-at-point))
                              "\""))
                  (eopengrok-find-definition (ivy-erlang-complete-thing-at-point)))))
             (define-key erlang-mode-map (kbd "C-c C-r")
               (lambda () (interactive)
                 (eopengrok-find-reference (ivy-erlang-complete-thing-at-point))))
             (define-key erlang-mode-map (kbd "C-c i")
               'fix-erlang-project-includes)
             (define-key erlang-mode-map (kbd "C-c b")
               'fix-erlang-project-code-path)))
(add-hook 'after-save-hook 'ivy-erlang-complete-reparse)

```

## Current state

For now project is useful but can contain some bugs. There are context
sensitive completions for:
* local functions
* module names
* module functions
* record names
* record fields
* macros names

And also can show documentation for functions from standart library in
your browser.

![gif](https://github.com/s-kostyaev/ivy-erlang-complete/raw/master/try.gif)

## Why not `distel`?

`distel` is great. I think `distel` much better then this package. But
I can't use it on my job for erlang developement. Only licensed nodes
can connect to cluster, distel nodes can not. I was trying to use esense
instead. But esense project is now dead and nobody fix crashes in
it. So I start this project.

## Contributing

Patches are welcome :)
