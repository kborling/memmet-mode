;;; memmet-mode.el --- Minimal Emmet Mode for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kevin Borling <kborling@protonmail.com>

;; Author: Kevin Borling
;; Created: April 11, 2025
;; Version: 0.0.1
;; Keywords: emmet, html
;; License: MIT
;; URL: https://github.com/kborling/memmet-mode
;; Homepage: https://github.com/kborling/memmet-mode
;; Filename: memmet.el
;; Package-Requires: ((emacs "24.1"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; `memmet-mode` is a minimal, fast, and Emacs-native implementation of
;; [Emmet](https://emmet.io/) for HTML editing. It supports a small but powerful
;; subset of Emmet syntax, enabling quick HTML tag generation.

;; Usage:

;; Type a minimal Emmet expression like `div#app>ul>li*3`,
;; place point at the end, and run `memmet-expand`

;;; Code:
(defun memmet-expand ()
  "Expand Emmet-style HTML at point with optional pretty-print and cursor placement."
  (interactive)
  (let* ((spec (memmet-extract-spec))
         (start (progn (skip-chars-backward "a-zA-Z0-9.#>*+-") (point)))
         (end   (progn (skip-chars-forward "a-zA-Z0-9.#>*+-") (point)))
         (raw-html (memmet-parse spec))
         (pretty-html (if (string-match-p ">" spec)
                          (memmet-pretty-format-html raw-html)
                        raw-html)))
    (delete-region start end)
    (insert pretty-html)
    ;; Move point inside the first tag
    (when (re-search-backward "<[^/][^>]*>" start t)
      (goto-char (match-end 0)))))

(defun memmet-extract-spec ()
  "Return the Emmet expression around point."
  (let ((start (progn (skip-chars-backward "a-zA-Z0-9.#>*+-") (point)))
        (end   (progn (skip-chars-forward "a-zA-Z0-9.#>*+-") (point))))
    (buffer-substring-no-properties start end)))

(defun memmet-parse (expr)
  "Parse an expression EXPR into HTML."
  (if (string-match-p "+" expr)
      ;; Handle siblings
      (mapconcat #'memmet-parse (split-string expr "+" t) "")
    ;; Handle parent > child
    (if (string-match "\\([^>]+\\)>\\(.+\\)" expr)
        (let ((parent (match-string 1 expr))
              (child (match-string 2 expr)))
          (memmet-wrap (memmet-parse-tag parent)
                       (memmet-parse child)))
      ;; No more nesting/siblings: parse tag
      (memmet-parse-tag expr))))

(defun memmet-parse-tag (expr)
  "Parse a single tag or repeated tag with optional class/id."
  (if (string-match "\\([^*]+\\)\\*\\([0-9]+\\)" expr)
      (let* ((base (match-string 1 expr))
             (count (string-to-number (match-string 2 expr))))
        (mapconcat (lambda (_) (memmet-tag-from-spec base)) (number-sequence 1 count) ""))
    (memmet-tag-from-spec expr)))

(defun memmet-tag-from-spec (spec)
  "Generate a tag from Emmet-style SPEC like `div.class` or `ul>li*5`. Defaults to div
   if only #id or .class is provided."
  (let ((tag "div")
        (class nil)
        (id nil))
    (message spec)
    ;; Extract class and ID first
    (when (string-match "\\.\\([a-zA-Z0-9-]+\\)" spec)
      (setq class (match-string 1 spec)))
    (when (string-match "#\\([a-zA-Z0-9-]+\\)" spec)
      (setq id (match-string 1 spec)))
    ;; Extract tag name if present before . or #
    ;; (when (string-match "\\`\\([a-zA-Z][a-zA-Z0-9-]*\\)" spec)
    (when (string-match "\\([a-zA-Z0-9-]+\\)" spec)
      (setq tag (match-string 1 spec)))

    (unless tag (setq tag "div"))
    ;; Return tag
    (format "<%s%s%s></%s>"
            tag
            (if id (format " id=\"%s\"" id) "")
            (if class (format " class=\"%s\"" class) "")
            tag)))

(defun memmet-wrap (outer inner)
  "Wrap INNER HTML content with OUTER tag."
  (if (string-match "\\`<\\([^ >]+\\)[^>]*>.*</\\1>\\'" outer)
      (replace-regexp-in-string
       (format "</%s>" (match-string 1 outer))
       (concat inner "</" (match-string 1 outer) ">")
       outer)
    outer))

(defun memmet-indent-lines (lines)
  "Indent LINES of HTML according to nesting, skipping tags with no children."
  (let ((indent 0)
        (output '())
        (stack '())
        (i 0)
        (n (length lines)))
    (while (< i n)
      (let* ((line (nth i lines))
             (trimmed (string-trim line))
             (next (if (< (1+ i) n) (string-trim (nth (1+ i) lines)) nil)))

        ;; Handle closing tag
        (when (string-match "\\`</\\([^>]+\\)>" trimmed)
          (setq indent (max 0 (- indent 2)))
          (pop stack))

        ;; Add line with current indent
        (push (concat (make-string indent ?\s) trimmed) output)

        ;; Handle opening tag
        (when (string-match "\\`<\\([^/! >]+\\)[^>]*>\\'" trimmed)
          (let ((tag-name (match-string 1 trimmed)))
            ;; Check if there's a closing tag on the next line
            (if (and next (string-match (format "\\`</%s>" tag-name) next))
                ;; Inline tag with its closing pair — overwrite previous output
                (let ((inline (concat trimmed next)))
                  (pop output)
                  (push (concat (make-string indent ?\s) inline) output)
                  (setq i (1+ i))) ;; Skip the next line
              ;; Otherwise it's a parent — indent
              (setq indent (+ indent 2))
              (push tag-name stack)))))
      (setq i (1+ i)))
    (nreverse output)))

(defun memmet-pretty-format-html (raw-html)
  "Format RAW-HTML string into indented HTML if it contains nesting."
  (if (not (string-match "></[a-zA-Z]" raw-html))
      raw-html  ;; No nesting — keep it one line
    (let* ((lines (split-string raw-html "><"))
           (lines (cl-mapcar (lambda (s i)
                               (cond
                                ((= i 0) (concat s ">"))
                                ((= i (1- (length lines))) (concat "<" s))
                                (t (concat "<" s ">"))))
                             lines
                             (number-sequence 0 (1- (length lines)))))
           (indented-lines (memmet-indent-lines lines)))
      (string-join indented-lines "\n"))))

;;;###autoload
(define-minor-mode memmet-mode
  "Minor mode for minimal emmet."
  :lighter "Memmet")

(provide 'memmet-mode)
;;; memmet-mode.el ends here
