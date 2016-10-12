;;; ivy-erlang-complete.el --- Erlang completion at point using ivy.


;; Copyright (C) 2016 Sergey Kostyaev

;; Author: Sergey Kostyaev <feo.me@ya.ru>
;; Version: 1.0.0
;; Keywords: erlang ivy completion
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `ivy-erlang-complete' is context sensitive erlang completion package with `ivy' as frontend.

;;; Code:
(require 'ivy)
(require 'subr-x)
(require 'dash)
(require 's)
(require 'erlang)
(require 'imenu)
(require 'counsel)
(require 'simple)
(if (> emacs-major-version 24) (require 'xref))

(defconst ivy-erlang-complete--base (file-name-directory load-file-name))

(defvar ivy-erlang-complete-erlang-root "/usr/lib/erlang/"
  "Path to erlang root.")

(defvar ivy-erlang-complete-project-root nil
  "Path to erlang project root.")

(defvar-local ivy-erlang-complete--file-suffix "-a -G '\\.[eh]rl'"
  "Regular expression for erlang files (*.erl *.hrl)")

(defvar-local ivy-erlang-complete-candidates nil
  "Candidates for completion.")

(defvar-local ivy-erlang-complete-predicate nil
  "Completion predicate.")

(defvar-local ivy-erlang-complete-records nil
  "Records accessible in current buffer.")

(defvar-local ivy-erlang-complete-macros nil
  "Macros accessible in current buffer.")

(defvar-local ivy-erlang-complete--record-names nil
  "Record names accessible in current buffer.")

(defvar-local ivy-erlang-complete--local-functions nil
  "Local functions in current buffer.")

(defvar ivy-erlang-complete--comment-regexp
  "%.*$")

(defvar-local ivy-erlang-complete--record-parsing-in-progress nil
  "Sync variable for async records parsing.")

(defvar-local ivy-erlang-complete--macros-parsing-in-progress nil
  "Sync variable for async macros parsing.")

(defvar ivy-erlang-complete--behaviours
  (make-hash-table :test 'equal)
  "Memoizaton for behaviours.")

(defvar ivy-erlang-complete--global-project-root nil
  "Global variable for use with async commands.")

(defun ivy-erlang-complete--executable (name)
  "Return path to executable with NAME."
  (concat ivy-erlang-complete--base "bin/" name))


;;;###autoload
(defun ivy-erlang-complete-autosetup-project-root ()
  "Automatically setup erlang project root."
  (interactive)
  (setq-local ivy-erlang-complete-project-root
              (ivy-erlang-complete--find-root-by-deps))
  ivy-erlang-complete-project-root)

(defun ivy-erlang-complete--find-root-by-deps (&optional start-dir)
  "Find project root as directory with rebar dependencies start from START-DIR."
  (let ((dir (locate-dominating-file (or (expand-file-name ".." start-dir)
                                         default-directory)
                                     "rebar.config")))
    (if dir
        (let ((test-dir (expand-file-name
                         (shell-command-to-string
                          (format "cd %s && %s" dir
                                  (ivy-erlang-complete--executable
                                   "find-deps-dir"))) dir)))
          (or (if (and (file-exists-p test-dir)
                       (directory-files test-dir))
                  (expand-file-name dir)
                nil)
              (ivy-erlang-complete--find-root-by-deps dir)))
      (expand-file-name (or start-dir default-directory)))))

;;;###autoload
(defun ivy-erlang-complete-init ()
  "Config ivy-erlang-complete by default."
  (interactive)
  (ivy-erlang-complete-reparse)
  (define-key erlang-mode-map (kbd "C-:")
    'ivy-erlang-complete)
  (define-key erlang-mode-map (kbd "C-c C-h")
    'ivy-erlang-complete-show-doc-at-point)
  (define-key erlang-mode-map (kbd "C-c C-e")
    #'ivy-erlang-complete-set-project-root)
  (define-key erlang-mode-map (kbd "M-.")
    #'ivy-erlang-complete-find-definition)
  (define-key erlang-mode-map (kbd "M-?")
    #'ivy-erlang-complete-find-references)
  (define-key erlang-mode-map (kbd "C-c C-f")
    #'ivy-erlang-complete-find-spec)
  (if (> emacs-major-version 24)
      (define-key erlang-mode-map (kbd "M-,")
        #'xref-pop-marker-stack)
    (define-key erlang-mode-map (kbd "M-,")
      #'pop-global-mark))
  )
;;;###autoload
(defun ivy-erlang-complete-show-doc-at-point ()
  "Show doc for function from standart library."
  (interactive)
  (let ((thing (ivy-erlang-complete-thing-at-point)))
    (if (not (s-contains? ":" thing))
        (message "Can't find docs for %s." thing)
      (let* ((erl-thing (s-split ":" thing))
             (module (car erl-thing))
             (function (car (cdr erl-thing)))
             (arity (erlang-get-function-arity))
             (candidates (-map (lambda (s) (concat module ":" s))
                               (ivy-erlang-complete--find-functions module))))
        (if (not module)
            (message "module at point not found")
          (ivy-read "Counsel-erl cand:" candidates
                    :require-match t
                    :initial-input (if (not arity) thing
                                       (concat module ":" function "/"
                                               (format "%d" arity)))
                    :action (lambda (s)
                              (let ((result (s-split "[:/]" s)))
                                (browse-url
                                 (format
                                  "http://erlang.org/doc/man/%s.html#%s-%s"
                                  (nth 0 result)
                                  (nth 1 result)
                                  (nth 2 result)))))))))))

;; For test eval this
;; (progn
;;  (load-file "/home/feofan/.emacs.d/ivy-erlang-complete/ivy-erlang-complete.el")
;;  (ivy-erlang-complete--test-export-regexp))

(defvar ivy-erlang-complete--export-regexp
  "-export([[:space:]\n]*\\[[\n[:space:]]*[a-z/0-9\n[:space:]\,_]*][\n[:space:]]*).")

(defvar-local last-erlang-comment nil)
(defun copy-buffer-no-comments ()
  "Copy current buffer to new one with removing comments."
  (let* ((old-buf (buffer-name))
         (new-buf (concat "*" old-buf "*"))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (pos (point)))
    (if (buffer-live-p new-buf)
        (kill-buffer new-buf))
    (with-current-buffer (get-buffer-create new-buf)
      (insert content)
      (goto-char (point-min))
      (while (search-forward-regexp
              ivy-erlang-complete--comment-regexp last-erlang-comment t)
        (replace-match (make-string (length (match-string 0)) ?\ )))
      (goto-char pos))
    new-buf))

(defun ivy-erlang-complete--find-functions (module)
  "Find functions in MODULE."
  (if (not ivy-erlang-complete-project-root)
      (ivy-erlang-complete-set-project-root))
  (s-split
   "\n"
   (shell-command-to-string
    (s-join
     " "
     (list
      (ivy-erlang-complete--executable "exported-funcs.sh")
      module
      ivy-erlang-complete-project-root ivy-erlang-complete-erlang-root)))
   t))

(defun ivy-erlang-complete--find-modules ()
  "Find modules."
  (if (not ivy-erlang-complete-project-root)
      (ivy-erlang-complete-set-project-root))
  (-map (lambda (s) (concat s ":"))
        (s-split "\n"
                 (shell-command-to-string
                  (s-join " "
                          (list
                           "find" ivy-erlang-complete-project-root
                           ivy-erlang-complete-erlang-root
                           "-iname '*.erl' | xargs basename -a |"
                           "sed -e 's/\\.erl//g'")))
                 t)))

(defun ivy-erlang-complete--extract-records (file)
  "Extract all records from FILE."
  (if (not ivy-erlang-complete-project-root)
      (ivy-erlang-complete-set-project-root))
  (-map (lambda (s) (concat s ")."))
        (s-split
         ")\\."
         (shell-command-to-string
          (s-join " "
                  (list "find" ivy-erlang-complete-project-root "-name" file "|"
                        "xargs" "sed" "-n" "'/-record(/,/})./p'")))
         t)))

(defun ivy-erlang-complete--parse-record (record)
  "Parse RECORD and set it acessable in current buffer."
  (let ((res (read (shell-command-to-string
                    (concat
                     (ivy-erlang-complete--executable "parse-record")
                     " \"\"\"" record "\"\"\"")))))
    (ignore-errors (puthash (car res)
                            (cdr res) ivy-erlang-complete-records))))
(defun ivy-erlang-complete--find-local-functions ()
  "Find all local functions."
  (if (not ivy-erlang-complete--local-functions)
      (setq ivy-erlang-complete--local-functions
            (progn
              (let ((pos (point)))
                (imenu--make-index-alist)
                (goto-char pos))
              (-map (lambda (elem) (car elem)) imenu--index-alist))))
  ivy-erlang-complete--local-functions)

(defun ivy-erlang-complete--find-local-vars ()
  "Find local variables at point."
  (let* ((pos (point))
         (pos2 (progn (backward-word) (point)))
         (function-begin (search-backward-regexp "^[a-z]"))
         (search-string (buffer-substring-no-properties function-begin pos2)))
    (goto-char pos)
    (-remove (lambda (s)
               (-contains? ivy-erlang-complete-macros (concat "?" s)))
             (with-temp-buffer
               (insert search-string)
               (goto-char (point-min))
               (setq case-fold-search nil)
               (setq-local local-vars '())
               (while
                   (search-forward-regexp "[A-Z][A-Za-z_0-9]*" nil t)
                 (add-to-list 'local-vars (match-string 0)))
               local-vars))))

(defun ivy-erlang-complete-thing-at-point ()
  "Return the erlang thing at point, or nil if none is found."
  (when (thing-at-point-looking-at "\??#?['A-Za-z0-9_:]+")
    (match-string-no-properties 0)))

(defun ivy-erlang-complete-record-at-point ()
  "Return the erlang record at point, or nil if none is found."
  (when (thing-at-point-looking-at
         (format "#\\(%s\\)\\([\s-]*{[^{^}^#]*}?\\)*[.]*" erlang-atom-regexp) 500)
    (match-string-no-properties 0)))

(defun ivy-erlang-complete-export-at-point ()
  "Return the erlang export at point, or nil if none is found."
  (with-current-buffer (get-buffer-create (copy-buffer-no-comments))
    (when (thing-at-point-looking-at
           ivy-erlang-complete--export-regexp
           500)
      (let ((result (match-string-no-properties 0)))
        (kill-buffer)
        result))))

(defun ivy-erlang-complete--get-included-files ()
  "Get included files for current buffer."
  (-map (lambda (m) (concat (file-name-base (s-trim (car (-drop 1 m)))) ".hrl"))
        (s-match-strings-all
         "-include[_lib]*([:space:]*\"\\([^\"]+\\)"
         (buffer-substring-no-properties 1 (point-max)))))

;;;###autoload
(defun ivy-erlang-complete-reparse ()
  "Reparse macros and recors for completion in current buffer."
  (interactive)
  (if (s-equals? major-mode "erlang-mode")
      (progn
        (setq ivy-erlang-complete--local-functions nil)
        (ivy-erlang-complete--find-local-functions)
        (ivy-erlang-complete--async-parse-macros)
        (setq ivy-erlang-complete--behaviours
              (make-hash-table :test 'equal))
        (ivy-erlang-complete--async-parse-records))))

(defun ivy-erlang-complete--parse-records ()
  "Parse erlang records in FILE."
  (setq ivy-erlang-complete-records (make-hash-table :test 'equal))
  (-map
   'ivy-erlang-complete--parse-record
   (-flatten
    (-map 'ivy-erlang-complete--extract-records
          (append (ivy-erlang-complete--get-included-files)
                  (list
                   (file-name-nondirectory (buffer-file-name)))))))
  ivy-erlang-complete-records)

(defun ivy-erlang-complete--async-parse-records ()
  "Async parse erlang records for current buffer."
  (if (not ivy-erlang-complete--record-parsing-in-progress)
      (progn
        (setq ivy-erlang-complete--record-parsing-in-progress t)
        (async-start
         `(lambda ()
            ,(async-inject-variables "load-path")
            (require 'ivy-erlang-complete)
            (find-file ,(buffer-file-name))
            (setq ivy-erlang-complete-project-root ,ivy-erlang-complete-project-root)
            (setq eval-expression-print-length nil)
            (setq print-length nil)
            (prin1-to-string (ivy-erlang-complete--parse-records))
            )
         `(lambda (res)
            (with-current-buffer ,(buffer-name)
              (setq ivy-erlang-complete-records (read res))
              (setq ivy-erlang-complete--record-parsing-in-progress nil))
            (message "Erlang completions updated"))))))

(defun ivy-erlang-complete--get-record-names ()
  "Return list of acceptable record names."
  (if (not ivy-erlang-complete-records)
      (progn
        (ivy-erlang-complete-reparse)
        (message "Please wait for record parsing")
        nil)
    (setq ivy-erlang-complete--record-names nil)
    (maphash (lambda (key _)
               (push (concat "#" key) ivy-erlang-complete--record-names))
             ivy-erlang-complete-records)
    ivy-erlang-complete--record-names))

(defun ivy-erlang-complete--get-record-fields (record)
  "Return list of RECORD fields."
  (if (not ivy-erlang-complete-records)
      (progn
        (ivy-erlang-complete-reparse)
        (message "Please wait for record parsing")
        nil)
    (-map (lambda (s) (concat (car s)
                              (if (cdr s)
                                  (let ((type (concat
                                               "\t\t:: "
                                               (s-join " | "
                                                       (-flatten (cdr s))))))
                                    (set-text-properties 0 (length type)
                                                         '(face success) type)
                                    type))))
          (gethash record ivy-erlang-complete-records))))

(defun ivy-erlang-complete--extract-macros (file)
  "Extract erlang macros from FILE."
  (-uniq
   (-map (lambda (s)
           (concat "?"
                   (car
                    (s-split "("
                             (car
                              (s-split "," (s-chop-prefix "-define(" s)))))))
         (s-split "\n"
                  (s-trim
                   (shell-command-to-string
                    (s-join " "
                            (list
                             "find" ivy-erlang-complete-project-root "-name" file
                             "| xargs grep -h -e '^-define('"))))))))

(defun ivy-erlang-complete--get-macros ()
  "Return list of acceptable erlang macros."
  (if (not ivy-erlang-complete-macros)
      (setq ivy-erlang-complete-macros
            (-uniq
             (-flatten
              (append
               (list "?MODULE" "?MODULE_STRING" "?FILE" "?LINE" "?MACHINE")
               (-map
                'ivy-erlang-complete--extract-macros
                (append
                 (ivy-erlang-complete--get-included-files)
                 (list (concat (file-name-base (buffer-file-name))
                               "."
                               (file-name-extension (buffer-file-name)))))))))))
  ivy-erlang-complete-macros)

(defun ivy-erlang-complete--async-parse-macros ()
  "Async parse erlang macros for current buffer."
  (if (not ivy-erlang-complete--macros-parsing-in-progress)
      (progn
        (setq ivy-erlang-complete--macros-parsing-in-progress t)
        (async-start
         `(lambda ()
            ,(async-inject-variables "load-path")
            (require 'ivy-erlang-complete)
            (find-file ,(buffer-file-name))
            (setq ivy-erlang-complete-project-root ,ivy-erlang-complete-project-root)
            (setq eval-expression-print-length nil)
            (setq print-length nil)
            (prin1-to-string (ivy-erlang-complete--get-macros))
            )
         `(lambda (res)
            (with-current-buffer ,(buffer-name)
              (setq ivy-erlang-complete-macros (read res))
              (setq ivy-erlang-complete--macros-parsing-in-progress nil))
            (message "Erlang completions updated"))))))


;;;###autoload
(defun ivy-erlang-complete-set-project-root ()
  "Set root for current project."
  (interactive)
  (let
      ((dir
        (expand-file-name (read-directory-name
                           "Select project directory:" default-directory))))
    (setq ivy-erlang-complete-project-root dir)
    dir))

(defun ivy-erlang-complete--insert-candidate (candidate)
  "Insert CANDIDATE at point."
  (if (string-match "\\(.*\\)\t\t\\(::.*\\)" candidate)
      (let ((type (match-string-no-properties 2 candidate)))
        (progn
          (ivy-erlang-complete--insert-candidate
           (match-string-no-properties 1 candidate))
          (message type)))
    (if (ivy-erlang-complete-export-at-point)
        (ivy-completion-in-region-action candidate)
      (if (and (s-prefix? "?" candidate)
               (thing-at-point-looking-at "\?['A-Za-z0-9_:]+"))
          (ivy-completion-in-region-action (s-chop-prefix "?" candidate))
        (if (string-match "\\([^/]+\\)/\\([0-9]+\\)" candidate)
            (let ((arity (string-to-number
                          (substring candidate
                                     (match-beginning 2) (match-end 2)))))
              (ivy-completion-in-region-action
               (concat (replace-regexp-in-string "/[0-9]+" "" candidate)
                       "("
                       (make-string (if (= 0 arity) arity (- arity 1)) ?,)
                       ")"))
              (goto-char (- (point) arity)))
          (if (string-match ".*{}$" candidate)
              (progn
                (ivy-completion-in-region-action candidate)
                (goto-char (- (point) 1)))
            (ivy-completion-in-region-action candidate)))))))

(defun ivy-erlang-complete--get-export ()
  "Return formated exported functions with arity."
  (-map (lambda (x)
          (format "%s/%d" (car x) (cdr x)))
        (erlang-get-export)))

;;;###autoload
(defun ivy-erlang-complete ()
  "Erlang completion at point."
  (interactive)
  (let ((thing (ivy-erlang-complete-thing-at-point)))
    (if (and thing (string-match "#?\\([^\:]+\\)\:\\([^\:]*\\)" thing))
        (let ((erl-prefix (substring thing (match-beginning 1) (match-end 1))))
          (progn
            (setq ivy-erlang-complete-candidates
                  (ivy-erlang-complete--find-functions
                   erl-prefix))
            (setq ivy-erlang-complete-predicate
                  (string-remove-prefix (concat erl-prefix ":") thing))))
      (progn
        (if (ivy-erlang-complete-export-at-point)
            (progn
              (setq ivy-erlang-complete--local-functions nil)
              (setq ivy-erlang-complete-candidates
                    (-remove (lambda (el)
                               (-contains?
                                (ivy-erlang-complete--get-export) el))
                             (ivy-erlang-complete--find-local-functions))))
          (if
              (ivy-erlang-complete-record-at-point)
              (setq ivy-erlang-complete-candidates
                    (append
                     (ivy-erlang-complete--get-record-fields
                      (buffer-substring-no-properties
                       (match-beginning 1) (match-end 1)))
                     (ivy-erlang-complete--find-local-vars)
                     (ivy-erlang-complete--find-local-functions)
                     (ivy-erlang-complete--get-record-names)
                     (ivy-erlang-complete--find-modules)
                     (ivy-erlang-complete--get-macros)
                     ))
            (setq ivy-erlang-complete-candidates
                  (append
                   (ivy-erlang-complete--find-local-vars)
                   (ivy-erlang-complete--find-local-functions)
                   (ivy-erlang-complete--get-record-names)
                   (ivy-erlang-complete--find-modules)
                   (ivy-erlang-complete--get-macros)
                   ))))
        (setq ivy-erlang-complete-predicate (s-chop-prefix "?" thing)))))
  (when (looking-back ivy-erlang-complete-predicate (line-beginning-position))
    (setq ivy-completion-beg (match-beginning 0))
    (setq ivy-completion-end (match-end 0)))
  (ivy-read "erlang cand:" ivy-erlang-complete-candidates
            :initial-input ivy-erlang-complete-predicate
            :action #'ivy-erlang-complete--insert-candidate))
;;;##restruct
;;;###autoload
(defun ivy-erlang-complete--find-definition (thing)
  "Search THING definition in DIRECTORY-PATH."
  (cond
   ((s-contains? ":" thing);module:function
    (let* ((thing2 (s-split ":" thing))
           (module (car thing2))
           (func (car (cdr thing2))))
      (ivy-erlang-complete--find-def (concat module ".erl")
                                     (concat "^" func"("))))
   ((s-prefix? "?" thing);find macro
    (ivy-erlang-complete--find-def (append
                                    (ivy-erlang-complete--get-included-files)
                                    (list
                                     (file-name-nondirectory
                                      (buffer-file-name))))
     (concat "^-define(" (s-chop-prefix "?" thing))))
   ((setq record (ivy-erlang-complete-record-at-point));find record
    (ivy-erlang-complete--find-def (append
                                    (ivy-erlang-complete--get-included-files)
                                    (list
                                     (file-name-nondirectory
                                      (buffer-file-name))))
     (concat
      "^-record("
      (match-string-no-properties 1) ",")))
   ((thing-at-point-looking-at "-behaviour(\\([a-z_]+\\)).")
    (ivy-erlang-complete--find-def
     ".erl$"
     (concat "^-module(" (match-string-no-properties 1)  ").")))
   ((string-match-p "^[a-z]" thing);local function
    (ivy-erlang-complete--find-def
     (file-name-nondirectory (buffer-file-name))
     (concat "^" thing"(")))
   (t  (message "Can't find definition"))))

(defun ivy-erlang-complete--prepare-find-args (args)
  "Prepare ARGS for `ivy-erlang-complete--find-grep-spec-function'."
  (if (not (listp args)) (ivy-erlang-complete--prepare-find-args (list args))
  (s-join " -o "
          (-map (lambda (s) (format "-name '%s\\.erl'" s)) (-flatten args)))))

(defun ivy-erlang-complete--prepare-def-find-args (args)
  "Prepare ARGS for `ivy-erlang-complete--find-grep-spec-function'."
  (if (not (listp args)) (ivy-erlang-complete--prepare-def-find-args (list args))
  (s-join " -o "
          (-map (lambda (s) (format "-name '%s'" s)) (-flatten args)))))

(defun ivy-erlang-complete--find-grep-spec-function (string extra-args)
  "Grep in the project directory for STRING.
If non-nil, EXTRA-ARGS string is appended to command."
  (when (null extra-args)
    (setq-local extra-args "*"))
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((qregex (shell-quote-argument
                  (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string))))))
      (let ((cmd (format "find %s %s %s | xargs grep -H -n -e '^-callback %s(' -e '^-spec %s('"
                         ivy-erlang-complete-erlang-root ivy-erlang-complete--global-project-root
                         (ivy-erlang-complete--prepare-find-args extra-args)
                         qregex qregex)))
        (counsel--async-command cmd))
      nil)))

(ivy-set-display-transformer 'ivy-erlang-complete--find-grep-spec-function 'counsel-git-grep-transformer)
(counsel-set-async-exit-code 'ivy-erlang-complete--find-grep-spec-function 123 "No matches found")
(ivy-set-occur 'ivy-erlang-complete--find-grep-spec-function 'counsel-ag-occur)

(defun ivy-erlang-complete--find-grep-def-function (string extra-args)
  "Grep in the project directory for STRING.
If non-nil, EXTRA-ARGS string is appended to command."
  (when (null extra-args)
    (setq-local extra-args "*"))
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((qregex (shell-quote-argument
                  (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string))))))
      (let ((cmd (format "find %s %s %s | xargs grep -H -n -e \"\"\"%s\"\"\""
                         ivy-erlang-complete-erlang-root ivy-erlang-complete--global-project-root
                         (ivy-erlang-complete--prepare-def-find-args extra-args)
                         string)))
        (message "%s" cmd)
        (counsel--async-command cmd))
      nil)))

(ivy-set-display-transformer 'ivy-erlang-complete--find-grep-def-function 'counsel-git-grep-transformer)
(counsel-set-async-exit-code 'ivy-erlang-complete--find-grep-def-function 123 "No matches found")
(ivy-set-occur 'ivy-erlang-complete--find-def-spec-function 'counsel-ag-occur)

(defun ivy-erlang-complete--find-def (filename regex)
  "Find definition in FILENAME by REGEX."
  (setq ivy-erlang-complete--global-project-root ivy-erlang-complete-project-root)
  (ivy-read "find definition  "
                  (lambda (string)
                    (ivy-erlang-complete--find-grep-def-function string filename))
                  :initial-input regex
                  :dynamic-collection t
                  :action #'counsel-git-grep-action
                  :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
                  :caller 'ivy-erlang-complete--find-grep-def-function))

(defun ivy-erlang-complete--extract-behaviours (file)
  "Return FILE behaviours."
  (or (gethash file ivy-erlang-complete--behaviours nil)
      (puthash file (-map
                     (lambda (s)
                       (string-match "-behaviour(\\([^\.]+\\))\." s)
                       (match-string 1 s))
                     (s-split "\n"
                              (shell-command-to-string
                               (format "grep -e '^-behaviour(' %s" file))
                              t))
               ivy-erlang-complete--behaviours)))

(defun ivy-erlang-complete--find-spec (thing)
  "Search spec for THING."
  (setq ivy-erlang-complete--global-project-root ivy-erlang-complete-project-root)
  (if (s-contains? ":" thing)
      (let* ((splitted-thing (s-split ":" thing))
             (module (car splitted-thing))
             (func (cadr splitted-thing)))
        (ivy-read "find spec  "
                  (lambda (string)
                    (ivy-erlang-complete--find-grep-spec-function string module))
                  :initial-input func
                  :dynamic-collection t
                  :action #'counsel-git-grep-action
                  :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
                  :caller 'ivy-erlang-complete--find-grep-spec-function))
    (let ((file (buffer-file-name)))
     (ivy-read "find spec  "
                  (lambda (string)
                    (ivy-erlang-complete--find-grep-spec-function
                     string
                     (append (ivy-erlang-complete--extract-behaviours
                              file)
                             (list (file-name-base file)))))
                  :initial-input thing
                  :dynamic-collection t
                  :action #'counsel-git-grep-action
                  :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
                  :caller 'ivy-erlang-complete--find-grep-spec-function))))

;;;###autoload
(defun ivy-erlang-complete-find-spec ()
  "Find spec at point.  It also find callback definition."
  (interactive)
  (if (> emacs-major-version 24)
      (xref-push-marker-stack)
    (push-mark))
  (ivy-erlang-complete--find-spec
   (ivy-erlang-complete-thing-at-point)))

;;;###autoload
(defun ivy-erlang-complete-find-definition ()
  "Find erlang definition."
  (interactive)
  (if (> emacs-major-version 24)
      (xref-push-marker-stack)
    (push-mark))
  (let ((thing (ivy-erlang-complete-thing-at-point)))
    (ivy-erlang-complete--find-definition thing)))

;;;###autoload
(defun ivy-erlang-complete-find-references ()
  "Find erlang references."
  (interactive)
  (if (> emacs-major-version 24)
      (xref-push-marker-stack)
    (push-mark))
  (let ((thing (ivy-erlang-complete-thing-at-point)))
    (if (s-contains? ":" thing)
        (counsel-ag (concat thing "(")
                    ivy-erlang-complete-project-root
                    ivy-erlang-complete--file-suffix "find references")
      (if (s-prefix? "?" thing)
          (counsel-ag (s-replace "?" "\\\?" thing)
                      ivy-erlang-complete-project-root
                      ivy-erlang-complete--file-suffix "find references")
        (if (thing-at-point-looking-at "-record(\\(['A-Za-z0-9_:.]+\\),")
            (counsel-ag (concat "#" (match-string-no-properties 1))
                        ivy-erlang-complete-project-root
                        ivy-erlang-complete--file-suffix "find references")
          (if (thing-at-point-looking-at "-define(\\(['A-Za-z0-9_:.]+\\),")
              (counsel-ag (concat "\\\?" (match-string-no-properties 1))
                          ivy-erlang-complete-project-root
                          ivy-erlang-complete--file-suffix "find references")
            (let ((record (ivy-erlang-complete-record-at-point)))
              (if record
                  (counsel-ag (concat "#" (match-string-no-properties 1))
                              ivy-erlang-complete-project-root
                              ivy-erlang-complete--file-suffix "find references")
                (if (thing-at-point-looking-at "-behaviour(\\([a-z_]+\\)).")
                    (counsel-ag (concat "^" (match-string-no-properties 0))
                                ivy-erlang-complete-project-root
                                "-a -G e\\.[eh]rl$" "find references")
                  (if (let* ((pos (point))
                             (is-function
                              (progn
                                (goto-char (line-beginning-position))
                                (or (search-forward-regexp
                                     (concat "^" thing "(")
                                     (point-max) t 1)
                                    (search-backward-regexp
                                     (concat "^" thing "(")
                                     (point-min) t 1)))))
                        (goto-char pos)
                        is-function)
                      (counsel-ag (concat (file-name-base (buffer-file-name))
                                          ":" thing "(")
                                  ivy-erlang-complete-project-root
                                  ivy-erlang-complete--file-suffix "find references")
                    (counsel-ag thing
                                ivy-erlang-complete-project-root
                                ivy-erlang-complete--file-suffix
                                "find references")))))))))))

;;; Testing
(defun ivy-erlang-complete--test-regexp (name re match-data unmatch-data)
  "Test with NAME for RE that must match MATCH-DATA and must not match UNMATCH-DATA."
  (-map (lambda (s)
          (if (string-match-p re s)
              (message "pass %s regexp" name)
            (message "fail %s regexp must match: %s" name s)))
        (-map (lambda (s) (replace-regexp-in-string
                           ivy-erlang-complete--comment-regexp
                           "" s)) match-data))
  (-map (lambda (s)
          (if (string-match-p re s)
              (message "fail %s regexp must not match: %s" name s)
            (message "pass %s regexp" name)))
        (-map (lambda (s) (replace-regexp-in-string
                           ivy-erlang-complete--comment-regexp
                           "" s)) unmatch-data)))

(defconst ivy-erlang-complete--matched-export-data
  '("-export([add/2, hello/0, greet_and_add_two/1])."
    "-export([add/2,
    hello/0,
    greet_and_add_two/1])."
    "-export(
[
    add/2,
\t  hello/0,
    greet_and_add_two/1])."
    "-export( [add/2, hello/0, greet_and_add_two/1] )."
    "-export([
    add/2,
    hello/0,
    greet_and_add_two/1
  ]
)."
    "-export(
  [
%
    add/2,
    hello/0,
    greet_and_add_two/1
  ])."
    "-export(
  [
%
    add/2, %% some
   %% hello/0,
    greet_and_add_two/1
    ])."
"-export(
  [
    add/2, %% some
   %% hello/0,
    greet_and_add_two/1
  ])."))

(defconst ivy-erlang-complete--unmatched-export-data
  '("-export([
    add/2,
\t  hello/0,
   % greet_and_add_two/1])."
    "-export( add/2, hello/0, greet_and_add_two/1] )."
    "-export(
  %[
    add/2,
    hello/0,
    greet_and_add_two/1
  ]
)."
    "-export(
  [
%
    add/2,
    hello/0,
    greet_and_add_two/1
)."
    "-export(
  [
%
    add/2, %% some
   %% hello/0,
    greet_and_add_two/1
  ]
% )."))

(defun ivy-erlang-complete--test-export-regexp ()
  "Testing export regexp."
  (ivy-erlang-complete--test-regexp
   "export" ivy-erlang-complete--export-regexp
   ivy-erlang-complete--matched-export-data
   ivy-erlang-complete--unmatched-export-data))

(provide 'ivy-erlang-complete)
;;; ivy-erlang-complete.el ends here
