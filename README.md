# ivy-erlang-complete


[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/ivy-erlang-complete-badge.svg)](https://melpa.org/#/ivy-erlang-complete)
[![MELPA Stable](https://stable.melpa.org/packages/ivy-erlang-complete-badge.svg)](https://stable.melpa.org/#/ivy-erlang-complete)


`ivy-erlang-complete` is context sensitive completion for erlang
without connecting to erlang nodes. It also can go to
definition/references/specs and find files in current project. Project
will be setup automatically based on rebar or rebar3 configs and some
heuristics. For now this package also support eldoc.

## Installation

You can now install package `ivy-erlang-complete` from
[MELPA](https://melpa.org/#/getting-started). Just `M-x`
`package-install`<kbd>Enter</kbd> `ivy-erlang-complete` <kbd>Enter</kbd>.

### System packages
 * findutils
 * coreutils
 * sed
 * grep
 * the_silver_searcher `ag` (for finding definition, references and specs)
 
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
* `flycheck` for on the fly error checking (will setup automatically)
* `wrangler` for smart refactoring

This is my emacs config for erlang developement:

``` emacs-lisp
;;;; Erlang
(defun my-erlang-hook ()
  "Setup for erlang."
  (require 'wrangler)
  (ivy-erlang-complete-init)
  (defvar erlang-extended-mode-map)
  (define-key erlang-extended-mode-map (kbd "M-.") nil)
  (define-key erlang-extended-mode-map (kbd "M-,") nil)
  (define-key erlang-extended-mode-map (kbd "M-?") nil)
  (define-key erlang-extended-mode-map (kbd "(") nil))
(add-hook 'erlang-mode-hook #'my-erlang-hook)
(add-hook 'after-save-hook #'ivy-erlang-complete-reparse)

(add-to-list 'auto-mode-alist '("rebar\\.config$" . erlang-mode))
(add-to-list 'auto-mode-alist '("relx\\.config$" . erlang-mode))
(add-to-list 'auto-mode-alist '("system\\.config$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.app\\.src$" . erlang-mode))

;;; wrangler
;; install https://github.com/RefactoringTools/wrangler/blob/master/INSTALL
;; before usage
(add-to-list 'load-path "/usr/lib/erlang/lib/wrangler-1.2.0/elisp")
;(require 'wrangler)
; Some code inspection functionalities of Wrangler generate .dot
; files, which can be compiled and previewed in Emacs if the
; Graphviz-dot mode for Emacs is enabled.
;(load-library "graphviz-dot-mode")
```

### Default keybindings
By default next keybindings will be setup (can be disabled by setting
variable `ivy-erlang-complete-use-default-keys` to `nil`):

key | function | description
--- | -------- | -----------
`C-:` | `ivy-erlang-complete` | start completion at point
`C-c C-h` | `ivy-erlang-complete-show-doc-at-point` | show docs for function under point (from standart library) in your browser
`C-c C-e` | `ivy-erlang-complete-set-project-root` | manually setup project root for strange cases (if you find that case please [open issue](https://github.com/s-kostyaev/ivy-erlang-complete/issues/new))
`M-.` | `ivy-erlang-complete-find-definition` | go to definition
`M-?` | `ivy-erlang-complete-find-references` | go to references
`C-c C-f` | `ivy-erlang-complete-find-spec` | go to spec or callback definition
`M-,` | `xref-pop-marker-stack` | for emacs 25+ go back
`M-,` | `pop-global-mark` | for emacs 24 go back
`C-c C-o` | `ivy-erlang-complete-find-file` | find file in current project (preselect file or symbol under point)



## Current state

For now project is useful for production erlang development. There are
some minor issues with context identifying. In this cases accuracy
sacrificed for speed. Also comleting for functions in modules with
export_all directive doesn't work. I can implement it. But I don't
do it. This implementation well be very slow. Also I think that
export_all is bad practice.

There are context sensitive completions for:
* local variables
* local functions
* module names
* module functions
* record names
* record fields
* macros names
* guards
* types

![completions](https://github.com/s-kostyaev/ivy-erlang-complete/raw/master/completion.gif)

Also you can find:
* definition of:
  * functions
  * macros
  * records
  * types
* references of:
  * functions
  * records
  * macros
  * types

![go to definition references](https://github.com/s-kostyaev/ivy-erlang-complete/raw/master/gotodefref.gif)

* specs for:
  * functions
  * callbacks from used behaviours

![find spec](https://github.com/s-kostyaev/ivy-erlang-complete/raw/master/findspec.gif)

And see documentation for functions from standart library in your
browser.

## TODO
- [x] create [company backend](https://github.com/s-kostyaev/company-erlang)
- [x] add eldoc support
- [ ] improve working with macros (add arity and eldoc)
- [ ] renew gifs in this file

## Why not `distel`?

`distel` is great. I think `distel` much better then this package in
some cases. But I can't configure it for work with my job's projects. 
I thought that only licensed nodes can connect to cluster, distel
nodes can not. It was mistake. I was trying to use esense instead. But
esense project is now dead and nobody fix crashes in it. So I start
this project. For now this project can do all the thins that esense
can do and more.

## Why not `edts`?

`edts` need for project configuration for every project you
work with. Every day I work with many erlang projects. And I don't
like configuration process at all. So this package no need it - it
work out of the box. Also I don't use `edts` sufficient time so please
add your comparison here.

## Contributing

Patches are welcome :) You also can send your bugreports and feature
requests [here](https://github.com/s-kostyaev/ivy-erlang-complete/issues/new).
