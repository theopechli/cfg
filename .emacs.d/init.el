(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(term-mode-hook
		ansi-term-mode-hook
                shell-mode-hook
		eshell-mode-hook
	        treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq display-time-format "%F %R"
      display-time-default-load-average nil)

(display-time)

(load-theme 'wombat)

(setq font '"Noto Sans Mono")

;;keep cursor at same position when scrolling
;; (setq scroll-preserve-screen-position 1)
;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

(setq mouse-wheel-scroll-amount '(4 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; (set-face-attribute 'default nil :font "Noto Sans Mono" :height 100)

(require 'cl)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(defun toggle-font-weight-normal ()
  (interactive)
  (set-face-attribute 'default nil :font (font-candidate (concat font '"-10:weight=normal"))))

(defun toggle-font-weight-bold ()
  (interactive)
  (set-face-attribute 'default nil :font (font-candidate (concat font '"-10:weight=bold"))))

(toggle-font-weight-bold)

(progn
  (define-prefix-command 'font-weight-keymap)
  (define-key font-weight-keymap (kbd "n") 'toggle-font-weight-normal)
  (define-key font-weight-keymap (kbd "b") 'toggle-font-weight-bold)
  (global-set-key (kbd "C-c f w") 'font-weight-keymap))

(require 'ido)
(ido-mode 1)
(make-local-variable 'ido-decorations)
(setf (nth 2 ido-decorations) "\n")
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(require 'icomplete)
(icomplete-mode 1)
(setq icomplete-separator "\n")
(setq icomplete-hide-common-prefix nil)
(setq icomplete-in-buffer t)
(setq icomplete-delay-completions-threshold 0)
(setq icomplete-max-delay-chars 0)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

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
  (define-prefix-command 'edit-keymap)
  (define-key edit-keymap (kbd "s") 'sudo-edit)
  (global-set-key (kbd "C-c e") 'edit-keymap))

(progn
  (define-prefix-command 'shell-keymap)
  (define-key shell-keymap (kbd "e") 'eshell)
  (define-key shell-keymap (kbd  "a") 'ansi-term)
  (global-set-key (kbd "C-c s") 'shell-keymap))

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)

;; (use-package ivy
;;   :init
;;   (ivy-mode 1)
;;   :diminish
;;   ivy-mode
;;   :bind*
;;   (("C-s" . swiper-isearch)
;;    ("C-x b" . ivy-switch-buffer)
;;    ("C-c v" . ivy-push-view)
;;    ("C-c V" . ivy-pop-view))
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "(%d/%d) "))

;; (use-package counsel
;;   :init
;;   (counsel-mode 1)
;;   :diminish
;;   counsel-mode
;;   :bind*
;;   (("M-x" . counsel-M-x)
;;    ("C-x C-f" . counsel-find-file)
;;    ("M-y" . counsel-yank-pop)
;;    ("C-h f" . counsel-describe-function)
;;    ("C-h v" . counsel-describe-variable)
;;    ("C-h l" . counsel-find-library)
;;    ("C-h i" . counsel-info-lookup-symbol)
;;    ("C-h u" . counsel-unicode-char)
;;    ("C-h j" . counsel-set-variable)))

;; (use-package which-key
;;   :init
;;   (which-key-mode)
;;   :diminish
;;   which-key-mode
;;   :config
;;   (setq which-key-idle-delay 1))

;; (use-package magit)

;; (use-package company
;;   :after
;;   lsp-mode
;;   :hook
;;   (lsp-mode . company-mode)
;;   :bind
;;   (:map company-active-map
;;         ("<tab>" . company-complete-selection))
;;   (:map lsp-mode-map
;;         ("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0))

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
          '(80 . 80) '(100 . 100)))))

(toggle-transparency)
(toggle-transparency)

(defun light-theme ()
  (interactive)
  (disable-theme 'wombat))

(defun dark-theme ()
  (interactive)
  (load-theme 'wombat))

(progn
  (define-prefix-command 'theme-keymap)
  (define-key theme-keymap (kbd "t") 'toggle-transparency)
  (define-key theme-keymap (kbd "l") 'light-theme)
  (define-key theme-keymap (kbd "d") 'dark-theme)
  (global-set-key (kbd "C-c t") 'theme-keymap))

(defun mpc-toggle ()
  (interactive)
  (shell-command "mpc toggle"))

(defun mpc-next ()
  (interactive)
  (shell-command "mpc next"))

(defun mpc-previous ()
  (interactive)
  (shell-command "mpc prev"))

(defun mpc-clear ()
  (interactive)
  (shell-command "mpc clear"))

(defun mpc-update ()
  (interactive)
  (shell-command "mpc update"))

(defun mpc-delete ()
  (interactive)
  (shell-command "mpc del 0"))

(defun mpc-play-artist ()
  (interactive)
  (setq artist (read-string "Enter artist: "))
  (shell-command (concat "mpc ls | grep \"" artist "\" | mpc add")))

(progn
  (define-prefix-command 'mpc-keymap)
  (define-key mpc-keymap (kbd "t") 'mpc-toggle)
  (define-key mpc-keymap (kbd "n") 'mpc-next)
  (define-key mpc-keymap (kbd "p") 'mpc-previous)
  (define-key mpc-keymap (kbd "c") 'mpc-clear)
  (define-key mpc-keymap (kbd "u") 'mpc-update)
  (define-key mpc-keymap (kbd "d") 'mpc-delete)
  (define-key mpc-keymap (kbd "a") 'mpc-play-artist)
  (global-set-key (kbd "C-c m") 'mpc-keymap))

(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

(defun open-in-external-app ()
  "Open the file(s) at point with an external application."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (mapc
     (lambda (file-path)
       (let ((process-connection-type nil))
         (start-process "" nil "xdg-open" file-path)))
     file-list)))

(defun dired-view-file ()
  "In Dired, examine a file in view mode, returning to Dired when done.
When file is a directory, show it in this buffer if it is inserted.
Otherwise, display it in another buffer.
When file is an mp4 video, open it with mpv."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
		(or (and (cdr dired-subdir-alist)
				 (dired-goto-subdir file))
			(dired file))
	  (if (string= (mailcap-extension-to-mime (file-name-extension file t)) "video/mp4")
		  (start-process "" nil "mpv" file)
		(view-file file)))))

(defun wallpaper-set-wallpaper ()
  (start-process-shell-command
   "Wallpaper" nil "feh --bg-scale --randomize ~/Pictures/*.png"))

(defun wallpaper-toggle-cycle ()
  (interactive)
  (run-with-timer 0 10 #'wallpaper-set-wallpaper))

(find-file "~/Personal/Org/Browser/Bookmarks.org")

(defun create-dir-hooks ()
  (interactive)
  (make-directory "/sudo:root@localhost:/etc/pacman.d/hooks/" t))

(defun update-efi-boot-manager ()
  (interactive)
  (create-dir-hooks)
  (with-temp-file "/sudo:root@localhost:/etc/pacman.d/hooks/100-systemd-boot.hook"
	(insert
	"[Trigger]
Type = Package
Operation = Upgrade
Target = systemd

[Action]
Description = Updating systemd-boot
When = PostTransaction
Exec = /usr/bin/bootctl update")))

;; (require 'whitespace)
;; (setq whitespace-style '(face empty tabs lines-tail trailing))
;; (global-whitespace-mode t)

;; (defun checksum ()
;;   (interactive)
;;   (let ((choices '("md5" "sha1" "sha224" "sha256" "sha384" "sha512")))
;;     (setq x (ido-completing-read "Choose algorithm hash: " choices))
;;     (message "Result: %s"
;; 	     (secure-hash
;; 	      x
;; 	      (read-file-name "Enter file name: ")))))

;; (defun indent-correctly (&optional arg)
;;   "If at beginning indent line like prev line (tab if still at beginning).
;;    If at end insert a tab.
;;    If in whitespace to prev line's whitespace.
;;    Possibly should do '*' as whitespace.
;;    "
;;   (interactive "P")
;;   (cond ( arg
;;           (let ((spaces 4))
;;             (while (> spaces 0)
;;               (forward-char -1)
;;               (if (or (char-equal (following-char) ? )
;;                       (char-equal (following-char) ?\t))
;;                   (progn (forward-char 1)
;;                          (backward-delete-char-untabify 1))
;;                 (setq spaces 1))
;;               (setq spaces (1- spaces)))))
;;         ( (bolp)
;;           (delete-region
;;            (point) (progn (skip-chars-forward " \t") (point)))
;;           (insert
;;            (save-excursion
;;              (forward-line -1)
;;              (buffer-substring
;;               (progn (beginning-of-line) (point))
;;               (progn ;; (skip-chars-forward "*")
;;                 (skip-chars-forward " \t") (point)))))
;;           (if (and (bolp) (or ;; (eq last-input-char ?\t)
;;                            (eq last-input-event 'return)
;;                            't)) (insert "    ")))  ;; HACK. FIX THIS.
;;         ( (or (char-equal (following-char) ? )
;;               (char-equal (following-char) ?\t))
;;           (delete-region
;;            (point) (progn (skip-chars-forward " \t") (point)))
;;           (indent-relative))
;;         ( t
;;           (insert "    "))))
