(defun memmet-expand ()
  "Expand a minimal Emmet expression at point into HTML."
  (interactive)
  (let* ((spec (memmet-extract-spec))
         (start (progn (skip-chars-backward "a-zA-Z0-9.#>*+") (point)))
         (end   (progn (skip-chars-forward "a-zA-Z0-9.#>*+") (point))))
    (delete-region start end)
    (insert (memmet-parse spec))))

(defun memmet-extract-spec ()
  "Return the Emmet expression around point."
  (let ((start (progn (skip-chars-backward "a-zA-Z0-9.#>*+") (point)))
        (end   (progn (skip-chars-forward "a-zA-Z0-9.#>*+") (point))))
    (buffer-substring-no-properties start end)))

(defun memmet-parse (expr)
  "Parse a minimal Emmet expression into HTML."
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
  "Generate a tag from Emmet-style spec like `div.class#id`. Defaults to div."
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
    (when (string-match "\\`\\([a-zA-Z][a-zA-Z0-9-]*\\)" spec)
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
