;;; init-yaml.el --- YAML files configuration        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup (:and
        (+treesit-language-available-p 'yaml)
        yaml-ts-mode)
  (:override-major-mode yaml-mode)

  (:hook (lambda ()
           (visual-line-mode -1)
           (visual-fill-column-mode -1)))
  (:hook #'flymake-yamllint-setup
         (lambda () (auto-fill-mode -1)))

  (:file-match "\\.bu\\'")  ; Butane files (Fedora CoreOS config transpiler)

  ;; Set `indent-line-function' to `yaml-indent-line'. Ideally this
  ;; should be set automatically by `yaml-ts-mode'... when this is
  ;; fixed we can remove this block.
  (:also-load yaml-mode)
  (:hook (lambda ()
           (setq-local indent-line-function #'yaml-indent-line))))

;;; Ansible

(setup ansible
  (:with-map ansible-key-map
    (:documentation-handler tsp/ansible-browse-documentation))
  (:with-hook ansible-hook
    (:hook #'ansible-remove-font-lock)))

(defun +ansible-bounds-of-rule-at-point ()
  "Return a cons cell containing the start and end of the Ansible rule at point."
  (let* ((allowed-chars "[:alpha:].")
         (beg (save-excursion
                (skip-chars-backward allowed-chars)
                (point)))
         (end (save-excursion
                (skip-chars-forward allowed-chars)
                (point))))
    (when (string-match-p "[[:alpha:].]" (buffer-substring beg end))
      (cons beg end))))

(put 'ansible-rule 'bounds-of-thing-at-point #'+ansible-bounds-of-rule-at-point)

(defun tsp/ansible-browse-documentation (rule)
  "Open the documentation page of an Ansible RULE."
  (interactive (list (thing-at-point 'ansible-rule t)))
  (browse-url
   (format "https://docs.ansible.com/ansible/latest/collections/%s_module.html"
           (string-replace "." "/" rule))))

(provide 'init-yaml)
;;; init-yaml.el ends here
