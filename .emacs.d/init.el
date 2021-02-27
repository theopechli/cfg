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
		ansi-term-mode-hook
                shell-mode-hook
		eshell-mode-hook
	        treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(show-paren-mode t)

(set-face-attribute 'default nil :font "Noto Sans Mono" :height 100)

(defalias 'yes-or-no-p 'y-or-n-p)

(load-theme 'leuven)

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file.

URL `https://emacsredux.com/blog/2013/04/21/edit-files-as-root/'"
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(progn
  (define-prefix-command 'shell-keymap)
  (define-key shell-keymap (kbd "e") 'eshell)
  (define-key shell-keymap (kbd  "a") 'ansi-term)
  (global-set-key (kbd "C-c s") 'shell-keymap))

(progn
  (define-prefix-command 'edit-keymap)
  (define-key edit-keymap (kbd "s") 'sudo-edit)
  (global-set-key (kbd "C-c e") 'edit-keymap))

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

(use-package ivy
  :init
  (ivy-mode 1)
  :diminish
  ivy-mode
  :bind*
  (("C-s" . swiper-isearch)
   ("C-x b" . ivy-switch-buffer)
   ("C-c v" . ivy-push-view)
   ("C-c V" . ivy-pop-view))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :init
  (counsel-mode 1)
  :diminish
  counsel-mode
  :bind*
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("M-y" . counsel-yank-pop)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h l" . counsel-find-library)
   ("C-h i" . counsel-info-lookup-symbol)
   ("C-h u" . counsel-unicode-char)
   ("C-h j" . counsel-set-variable)))

(use-package which-key
  :init
  (which-key-mode)
  :diminish
  which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package magit)

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

(defun html-template ()
  (interactive)
  (append-to-file "<!DOCTYPE html>
<html lang=\"el-GR\">
<head>
  <title></title>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, viewport-fit=cover\">
  <meta name=\"description\" content=\"Description of the page less than 150 characters\">
  <link rel=\"icon\" type=\"image/png\" href=\"https://example.com/favicon.png\">
  <link rel=\"apple-touch-icon\" href=\"/custom-icon.png\">
  <meta name=\"apple-mobile-web-app-capable\" content=\"yes\">
  <meta name=\"apple-mobile-web-app-status-bar-style\" content=\"black\">
  <meta name=\"msapplication-config\" content=\"browserconfig.xml\" />
</head>
<body>
</body>
</html>
" nil (counsel-find-file))
  (revert-buffer :ignore-auto :noconfirm))

(defun xml-template-browserconfig ()
  (interactive)
  (append-to-file "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<browserconfig>
   <msapplication>
     <tile>
        <square70x70logo src=\"small.png\"/>
        <square150x150logo src=\"medium.png\"/>
        <wide310x150logo src=\"wide.png\"/>
        <square310x310logo src=\"large.png\"/>
     </tile>
   </msapplication>
</browserconfig>
" nil (counsel-find-file))
  (revert-buffer :ignore-auto :noconfirm))

(defun escape-quotes (@begin @end)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `unescape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" "FIXEDCASE" "LITERAL")))))

(defun unescape-quotes (@begin @end)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `escape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" "FIXEDCASE" "LITERAL")))))

(defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(85 . 50) '(100 . 100)))))

(defun light-theme ()
  (interactive)
  (disable-theme 'misterioso)
  (load-theme 'leuven))

(defun dark-theme ()
  (interactive)
  (disable-theme 'leuven)
  (load-theme 'misterioso))

(progn
  (define-prefix-command 'theme-keymap)
  (define-key theme-keymap (kbd "t") 'toggle-transparency)
  (define-key theme-keymap (kbd "l") 'light-theme)
  (define-key theme-keymap (kbd "d") 'dark-theme)
  (global-set-key (kbd "C-c t") 'theme-keymap))
