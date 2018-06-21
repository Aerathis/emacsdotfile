(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(ivy-mode 1)
(load-theme 'wombat)

(c-set-offset 'case-label '+)

(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'magit-status)

(setq-default indent-tabs-mode nil)

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

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "GOROOT"))

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  (flycheck-mode)
  (require 'golint))

(add-hook 'go-mode-hook 'my-go-mode-hook)
(setq mac-command-modifier 'meta)

(add-hook 'before-save-hook 'editorconfig-apply)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (flycheck-clojure cider cider-eval-sexp-fu editorconfig lice company-terraform terraform-mode yaml-mode toml-mode slime-company rainbow-mode rainbow-identifiers rainbow-delimiters rainbow-blocks racer paredit-everywhere neotree magit hindent hi2 groovy-mode golint go-guru flymake-jslint flymake-jshint flycheck-rust flycheck-haskell flycheck-elm fiplr exec-path-from-shell ensime elm-mode elein dockerfile-mode csharp-mode coverlay counsel dash company-racer company-go company-ghc cargo ac-cider)))
 '(sh-basic-offset 2)
 '(sh-indentation 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
