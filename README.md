# memmet-mode — Minimal Emmet Mode for Emacs

`memmet-mode` is a minimal, fast implementation of [Emmet](https://emmet.io/) for HTML editing. It supports a small but powerful subset of Emmet syntax, enabling quick HTML tag generation.

---

## Features

- Expand basic tag names:
  ```emacs
  div       → <div></div>
  span      → <span></span>
  ```
- Add `id` and `class` via shorthand:
  ```emacs
   div#main      → <div id="main"></div>
   #main         → <div id="main"></div>
   div.container → <div class="container"></div>
   ```
- Combine `id` and `class`:
  ```emacs
  section#hero.container → <section id="hero" class="container"></section>
  ```
- Nesting with `>`:
  ```emacs
  div>h3 → <div><h3></h3></div>
  ```
- Multiplication (`*`) for repeated elements:
  ```emacs
  ul>li*3 → <ul><li></li><li></li><li></li></ul>
  ```

## Usage

    Install the package manually and load it in your emacs init file.

    Enable memmet-mode in HTML buffers: 
    ```emacs
    (use-package memmet-mode
    :load-path "/path/to/memmet"
    :hook (html-mode . memmet-mode))
    ```
    
Use the expansion command:

`M-x memmet-expand`

Or bind it to a key, e.g.:
   ```emacs
    (define-key memmet-mode-map (kbd "C-c m e") #'memmet-expand)
    ```
    Type a minimal Emmet expression like `div#app>ul>li*3`, place point at the end, and run `memmet-expand`.
