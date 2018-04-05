(package-initialize)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(ivy-mode 1)
(load-theme 'misterioso)

(c-set-offset 'case-label '+)

(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'magit-status)

(setq-default indent-tabs-mode nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(js-switch-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'company-racer)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-elm))

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook #'flycheck-mode)

(add-hook 'elm-mode-hook #'flycheck-mode)
(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(add-hook 'elm-mode-hook #'company-mode)

(add-hook 'haskell-mode-hook #'company-mode)
(add-hook 'haskell-mode-hook #'hi2-mode)
(add-hook 'haskell-mode-hook #'flycheck-mode)
(add-hook 'haskell-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-ghc)))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
(setenv "GOPATH" "/home/jeff/workspace/go")

(add-to-list 'exec-path "/home/jeff/workspace/go/bin")


(defun my-go-mode-hook ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  (flycheck-mode)
  (require 'golint)
  (if (not (string-match "go" 'compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(add-hook 'go-mode-hook 'my-go-mode-hook)
