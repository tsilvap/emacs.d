;;; init-elpaca.el --- Elpaca configuration          -*- lexical-binding: t; -*-
;;; Commentary:

;; We use Elpaca instead of package.el due to its flexibility: we can
;; install packages from source instead of tarballs for easier
;; hacking, pin packages to specific archives or commits, etc.

;;; Code:

;;;; Elpaca bootstrap

(defvar elpaca-installer-version 0.3)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process "emacs" nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

;; (add-hook 'after-init-hook #'elpaca-process-queues) ; cf. end of this file

(elpaca `(,@elpaca-order))

;;;; end of bootstrap

;; Install and activate packages.
(dolist (elpaca-order (with-temp-buffer
                        (insert-file-contents
                         (concat user-emacs-directory "packages.el"))
                        (read (current-buffer))))
  (elpaca `(,@elpaca-order)))

;; Call `elpaca-process-queues' immediately instead of having it as an
;; `after-init-hook'. The advantage is we can assume all packages are
;; already installed in the rest of the init code. But we mustn't call
;; `elpaca' directly anywhere else (instead, add packages/recipes to
;; the "packages.el" file).
(elpaca-process-queues)

(provide 'init-elpaca)
;;; init-elpaca.el ends here
