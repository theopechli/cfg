(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
	        treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(show-paren-mode t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)

(load-theme 'wombat)

(defalias 'yes-or-no-p 'y-or-n-p)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package diminish)

(use-package doom-themes
  :init
  (load-theme 'doom-one t))

(use-package all-the-icons)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  ((doom-modeline-height 15)))

(use-package ivy
  :init
  (ivy-mode 1)
  :diminish
  ivy-mode
  :bind
  (("C-s" . swiper))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :init
  (counsel-mode 1)
  :diminish
  counsel-mode
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)))

(use-package which-key
  :init
  (which-key-mode)
  :diminish
  which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package projectile
  :init
  (when (file-directory-p "~/Projects/Git/")
    (setq projectile-project-search-path '("~/Projects/Git")))
  (setq projectile-switch-project-action #'projectile-dired)
  :diminish
  projectile-mode
  :config
  (projectile-mode)
  :custom
  ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package magit)

(use-package treemacs)

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :commands
  (lsp lsp-deferred))

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode))
  ;; :custom
  ;; (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs)

(use-package lsp-ivy)

(use-package dap-mode
  :after
  lsp-mode
  :config
  (dap-auto-configure-mode))

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package company
  :after
  lsp-mode
  :hook
  (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp)
  (require 'dap-java))
