;;; doctor-checkup.el --- Detect missing features     -*- lexical-binding: t; -*-
;;; Commentary:

;; Inspired by Doom Emacs's "doom doctor" command.

;;; Code:

(defvar +doctor/recommended-version "29")

;; Borrowed from: https://github.com/garyo/emacs-config/blob/00e227f23e45290f2d4149ed57ed1401318cb9dd/emacs-config.org#detect-emacs-features
(defun +doctor/fast-json-available-p ()
  "Return t if \"json-serialize\" is implemented as a C function.
This was done for Emacs 27 but not all builds include the C version,
which is a lot faster."
  (and
   (subrp (symbol-function 'json-serialize))
   ;; Test that it works -- on Windows the DLL (or deps) may be missing.
   (equal (json-serialize (json-parse-string "[123]")) "[123]")))

;;;; Checkups

(unless (version<= +doctor/recommended-version emacs-version)
  (warn (format "This Emacs version (%s) is below the recommended version (%s). Some optional features may not work or may have suboptimal performance."
                emacs-version +doctor/recommended-version)))

(unless (+doctor/fast-json-available-p)
  (warn "This Emacs is using older Elisp JSON functions; maybe rebuild with libjansson?"))

(unless (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
  (warn "This Emacs does not support native compilation; maybe rebuild with --with-native-compilation?"))

(unless (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  (warn "This Emacs does not support tree-sitter; maybe rebuild with --with-tree-sitter?"))

(unless (executable-find "rg")
  (warn "Ripgrep executable (rg) not found on the system. Consider installing it to speed up directory searches: https://github.com/BurntSushi/ripgrep"))

(unless (executable-find "mdl")
  (warn "Markdownlint executable (mdl) not found on the system. Consider installing it if you want Markdown linting: https://github.com/markdownlint/markdownlint"))

(provide 'doctor-checkup)
;;; doctor-checkup.el ends here
