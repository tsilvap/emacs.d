;;; init-setup.el --- Setup.el configuration         -*- lexical-binding: t; -*-
;;; Commentary:

;; A lot of these macros were taken from:
;; https://www.emacswiki.org/emacs/SetupEl

;;; Code:

(require 'setup)

(defmacro setc (&rest args)
  "Customize user options using ARGS like `setq'."
  (declare (debug setq))
  `(setup (:option ,@args)))

(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature))))))
  :documentation "Autoload non-interactive FUNC, if it's not already bound."
  :repeatable t)

(setup-define :commands
  (lambda (command)
    (let ((cmd (if (memq (car-safe command) '(quote function))
                   (cadr command)
                 command)))
      `(unless (fboundp (quote ,cmd))
         (autoload (function ,cmd) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND, if it's not already bound."
  :repeatable t)

(setup-define :gkey
  (lambda (key command)
    `(keymap-global-set ,key ',command))
  :documentation "Globally bind KEY to COMMAND."
  :after-loaded t
  :repeatable t)

(setup-define :hide-mode
  (lambda (&optional mode)
    (let* ((mode (or mode (setup-get 'mode)))
           (mode (if (string-match-p "-mode\\'" (symbol-name mode))
                     mode
                   (intern (format "%s-mode" mode)))))
      `(setq minor-mode-alist
             (delq (assq ',mode minor-mode-alist)
                   minor-mode-alist))))
  :documentation "Hide the mode-line lighter of the current mode.
Alternatively, MODE can be specified manually, and override the
current mode."
  :after-loaded t)

(setup-define :key
  (lambda (key command)
    `(keymap-set ,(setup-get 'map) ,key ',command))
  :documentation "Bind KEY to COMMAND in current map."
  :after-loaded t
  :repeatable t)

(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :documentation-handler
  (lambda (handler)
    `(:bind "C-c c k" ,handler))
  :documentation "Set HANDLER as the documentation handler in current mode."
  :ensure '(func))

(setup-define :face
  (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
  :documentation "Customize FACE to SPEC."
  :repeatable t
  :after-loaded t)

(setup-define :flymake-flycheck-backend
  (lambda (checker)
    `(:local-hook flymake-diagnostic-functions
                  (flymake-flycheck-diagnostic-function-for ',checker)))
  :documentation "Set the Flycheck CHECKER as a Flymake backend in current mode."
  :repeatable t)

(setup-define :fullframe
  (lambda (on-commands off-command)
    `(progn
       ,@(mapcar (lambda (on-command)
                   `(fullframe ,on-command ,off-command))
                 (if (listp on-commands)
                     on-commands
                   (list on-commands)))))
  :documentation "Advise ON-COMMANDS to open their buffers in a full-frame window.
The previous window configuration is restored after calling
OFF-COMMMAND.")

(setup-define :localleader
  (lambda (key command)
    `(:key ,(concat "C-c l " key) ,command))
  :documentation "Bind KEY to COMMAND in localleader map."
  :repeatable t)

(setup-define :override-major-mode
  (lambda (mode)
    (let ((new-mode (setup-get 'mode)))
      `(add-to-list 'major-mode-remap-alist '(,mode . ,new-mode))))
  :documentation "Override the major mode MODE with the current mode."
  :ensure '(func))

(provide 'init-setup)
;;; init-setup.el ends here
