;;; ivy-erlang-complete.el --- Erlang completion at point using ivy.


;; Copyright (C) 2016 Sergey Kostyaev

;; Author: Sergey Kostyaev <feo.me@ya.ru>
;; Version: 1.0.0
;; Keywords: erlang ivy completion
;; Package-Requires: ((emacs "24.4") (ivy "0.8.0") (dash "2.12.1") (s "1.11.0"))

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

;; `ivy-erlang-complete' is erlang completion package with `ivy' as frontend.

;;; Code:
(require 'ivy)
(require 'subr-x)
(require 'dash)
(require 's)

(defvar ivy-erlang-complete-erlang-root "/usr/lib/erlang"
  "Path to erlang root.")

(defvar ivy-erlang-complete-project-root nil
  "Path to erlang project root.")

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

(defun ivy-erlang-complete--find-functions (module)
  "Find functions in MODULE."
  (if (not ivy-erlang-complete-project-root)
      (ivy-erlang-complete-set-project-root))
  (s-split "\n"
   (shell-command-to-string
    (s-join " "
     (list
      "find" ivy-erlang-complete-project-root ivy-erlang-complete-erlang-root
      "-name" (concat module ".erl") "| xargs sed -n '/-export(/,/)./p'"
      "| sed -e '/%/d' | sed -e 's/ //g' | sed -e 's/\\t//g'"
      "| sed -e '/^$/d' | sed -e '/-export(\\[.*\\])./{ n ; d }'"
      "| sed -e 's/-export.*(\\\[//g' | sed -e 's/\\\]).//g'"
      "| sed 's/\\\,/\\\n/g' | sed '/^$/d'")))
    t))

(defun ivy-erlang-complete--find-modules ()
  "Find functions in MODULE."
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
  (-map (lambda (s) (concat (replace-regexp-in-string "=[^\n]+\n" ",\n"
                             (replace-regexp-in-string "::[^\n]+\n?" ",\n" s))
                            "})."))
   (s-split
    ")\\."
    (shell-command-to-string
     (s-join " "
             (list "find" ivy-erlang-complete-project-root "-name" file "|"
                   "xargs" "sed" "-n" "'/-record(/,/})./p'" "|"
                   "sed -e 's/%.*//g'")))
    t)))

(defun ivy-erlang-complete--parse-record (record)
  "Parse RECORD and set it accessible in current buffer."
  (if (not ivy-erlang-complete-records)
      (setq ivy-erlang-complete-records (make-hash-table :test 'equal)))
  (let ((matched
         (-map 's-trim
               (-drop 1 (s-match "-record(\\([^,]+\\),[^{]*{\\(.*\\)}*."
                                 (s-collapse-whitespace record))))))
    (if matched
        (puthash (car matched)
                 (-remove (lambda (s)
                            (or (string-empty-p s)
                                (string-match-p "^[})[:space:]=]+$" s)))
                          (-map 's-trim (s-split "," (car (cdr matched)))))
                 ivy-erlang-complete-records)
      )))

(defun ivy-erlang-complete--extract-functions (file)
  "Extract all functions from FILE."
  (s-split "\n"
    (shell-command-to-string
     (s-join " "
      (list
       "sed -n '/^[a-z][a-zA-Z0-9_]*(.*)/,/[[:space:]]*->/p' " file
       " | sed -e '/%/d' | sed -e '/^\\\-/d' | sed -e '/^[[:space:]]/d'"
       "| sed '/^$/d' | sed -e 's/).*/)/g'")))
    t))

(defun ivy-erlang-complete--set-arity (erl-function)
  "Set arity to ERL-FUNCTION instead of arglist."
  (let ((arity
         (format "%d"
          (length
           (-drop-while 'string-empty-p
            (-map 's-trim
                  (s-split
                   ","
                   (replace-regexp-in-string
                    ")" ""
                    (replace-regexp-in-string
                     "[^(]+(" ""
                     (s-collapse-whitespace erl-function)))
                   t)))))))
    (when
        (string-match "[^(]+" erl-function)
      (concat (substring erl-function (match-beginning 0) (match-end 0))
              "/" arity))))

(defun ivy-erlang-complete--find-local-functions ()
  "Find all local functions."
  (-map #'ivy-erlang-complete--set-arity
          (ivy-erlang-complete--extract-functions (buffer-file-name))))

(defun ivy-erlang-complete-thing-at-point ()
  "Return the erlang thing at point, or nil if none is found."
  (when (thing-at-point-looking-at "['A-Za-z0-9_#:]+")
    (match-string-no-properties 0)))

(defun ivy-erlang-complete-record-at-point ()
  "Return the erlang record at point, or nil if none is found."
  (when (thing-at-point-looking-at "#\\('?[a-zA-z0-9_]+'?\\){[^{^}]+" 500)
    (match-string-no-properties 0)))

(defun ivy-erlang-complete-export-at-point ()
  "Return the erlang export at point, or nil if none is found."
  (when (thing-at-point-looking-at
         "-export([\n[:space:]]*\[[a-z0-9_/,[:space:]\n]*\][\n[:space:]]*)\."
         500)
    (match-string-no-properties 0)))

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
        (setq ivy-erlang-complete-macros nil)
        (ivy-erlang-complete--get-macros)
        (setq ivy-erlang-complete-records nil)
        (-map
         'ivy-erlang-complete--parse-record
         (-flatten
          (-map 'ivy-erlang-complete--extract-records
                (ivy-erlang-complete--get-included-files))))
        (-map
         'ivy-erlang-complete--parse-record
         (ivy-erlang-complete--extract-records (concat
                                                (file-name-base
                                                 (buffer-file-name))
                                                "."
                                                (file-name-extension
                                                 (buffer-file-name)))))
        (message "Erlang completions updated"))))

(defun ivy-erlang-complete--get-record-names ()
  "Return list of acceptable record names."
  (if (not ivy-erlang-complete-records)
      (ivy-erlang-complete-reparse))
  (setq ivy-erlang-complete--record-names nil)
  (maphash (lambda (key _)
             (push (concat "#" key "{ }") ivy-erlang-complete--record-names))
           ivy-erlang-complete-records)
  ivy-erlang-complete--record-names)

(defun ivy-erlang-complete--get-record-fields (record)
  "Return list of RECORD fields."
  (-map (lambda (s) (concat s " = "))
        (gethash record ivy-erlang-complete-records)))

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
                             "find ~/chronica -name" file
                             "| xargs grep -e 'define('"))))))))

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
  (if (ivy-erlang-complete-export-at-point)
      (progn
        (ivy-completion-in-region-action candidate))
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
     (if (string-match ".*{ }$" candidate)
         (progn
           (ivy-completion-in-region-action candidate)
           (goto-char (- (point) 2)))
       (ivy-completion-in-region-action candidate)))))

;;;###autoload
(defun ivy-erlang-complete ()
  "Erlang completion at point."
  (interactive)
  (let ((thing (ivy-erlang-complete-thing-at-point)))
   (if (and thing (string-match "\\([^\:]+\\)\:\\([^\:]*\\)" thing))
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
             (setq ivy-erlang-complete-candidates
                  (ivy-erlang-complete--find-local-functions)))
        (if
            (ivy-erlang-complete-record-at-point)
            (setq ivy-erlang-complete-candidates
                  (append
                   (ivy-erlang-complete--get-record-fields
                    (buffer-substring-no-properties
                     (match-beginning 1) (match-end 1)))
                   (ivy-erlang-complete--find-local-functions)
                   (ivy-erlang-complete--get-record-names)
                   (ivy-erlang-complete--find-modules)
                   (ivy-erlang-complete--get-macros)
                   ))
          (setq ivy-erlang-complete-candidates
                (append
                 (ivy-erlang-complete--find-local-functions)
                 (ivy-erlang-complete--get-record-names)
                 (ivy-erlang-complete--find-modules)
                 (ivy-erlang-complete--get-macros)
                 ))))
       (setq ivy-erlang-complete-predicate thing))))
  (when (looking-back ivy-erlang-complete-predicate (line-beginning-position))
    (setq ivy-completion-beg (match-beginning 0))
    (setq ivy-completion-end (match-end 0)))
  (ivy-read "Counsel-erl cand:" ivy-erlang-complete-candidates
            :initial-input ivy-erlang-complete-predicate
            :action #'ivy-erlang-complete--insert-candidate))



(provide 'ivy-erlang-complete)
;;; ivy-erlang-complete.el ends here
