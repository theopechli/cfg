(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

(load-theme 'wombat)

(dolist (mode '(term-mode-hook
				ansi-term-mode-hook
                shell-mode-hook
				eshell-mode-hook
				vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq display-time-format "%F %R"
      display-time-default-load-average nil)

(display-time)

;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

(setq mouse-wheel-scroll-amount '(4 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(set-face-attribute 'default nil
					:family "DejaVu Sans Mono"
					:height 105
					:weight 'regular)

(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

(setq calendar-week-start-day 1)

(with-eval-after-load 'dired
  (define-key dired-mode-map "k" 'kill-this-buffer))

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

(setq org-adapt-indentation nil)
(setq org-startup-folded t)
(setq org-ellipsis " ▾")
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
;; (set-face-attribute 'org-headline-done nil :strike-through t)
(setq org-agenda-files
	  '("~/Personal/Org/Tasks.org"))

(progn
  (define-prefix-command 'org-keymap)
  (define-key org-keymap (kbd "a") 'org-agenda)
  (global-set-key (kbd "C-c o") 'org-keymap))

(require 'package)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(which-key eglot rainbow-mode system-packages))
 (unless (package-installed-p package)
   (package-install package)))

(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 1)

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(require 'system-packages)
(progn
  (define-prefix-command 'system-packages-keymap)
  (define-key system-packages-keymap (kbd "u") 'system-packages-update)
  (define-key system-packages-keymap (kbd "s") 'system-packages-search)
  (define-key system-packages-keymap (kbd "i") 'system-packages-install)
  (define-key system-packages-keymap (kbd "l") 'system-packages-log)
  (global-set-key (kbd "C-c s") 'system-packages-keymap))

(require 'image-dired)
(setq image-dired-external-viewer "feh")

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
  (define-key shell-keymap (kbd  "v") 'vterm)
  (global-set-key (kbd "C-c c") 'shell-keymap))

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

(defun disable-all-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun default-theme ()
  (interactive)
  (disable-all-themes))

(defun wombat-theme ()
  (interactive)
  (disable-all-themes)
  (load-theme 'wombat))

(defun leuven-theme ()
  (interactive)
  (disable-all-themes)
  (load-theme 'leuven))

(progn
  (define-prefix-command 'theme-keymap)
  (define-key theme-keymap (kbd "t") 'toggle-transparency)
  (define-key theme-keymap (kbd "d") 'default-theme)
  (define-key theme-keymap (kbd "l") 'leuven-theme)
  (define-key theme-keymap (kbd "w") 'wombat-theme)
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

(defun mpc-repeat ()
  (interactive)
  (shell-command "mpc repeat"))

(defun mpc-single ()
  (interactive)
  (shell-command "mpc single"))

(defun mpc-skip-forward ()
  (interactive)
  (shell-command "mpc seek +0:0:10"))

(defun mpc-skip-backward ()
  (interactive)
  (shell-command "mpc seek -0:0:10"))

(defun mpc-add-song ()
  (interactive)
  ;; (setq song (file-relative-name (ido-read-file-name "Play song: " "~/Music/") "~/Music/"))
  (setq songs
		(let (value)
		  (dolist (element (directory-files "~/Music" nil "m4a") value)
			(setq value (cons element value)))))
  (setq song (ido-completing-read "Play song: " songs))
  (dolist (element songs nil)
	(when (string= element song)
	  (shell-command (format "%s\"%s\"" "mpc add " song)))))

(defun mpc-add-artist ()
  (interactive)
  (setq artists
		(let (value)
		  (dolist (element (directory-files "~/Music" nil "m4a") value)
			(setq value (cons (substring element 0 (string-match " -" element)) value)))))
  (delq nil (delete-dups artists))
  (setq artist (ido-completing-read "Play artist: " artists))
  (dolist (element artists nil)
	(when (string= element artist)
	  (shell-command (concat "mpc ls" " | grep \"" element "\" | mpc add")))))

(defun mpc-download ()
  (interactive)
  (setq link (read-string "Enter link: "))
  (async-shell-command (concat "youtube-dl --audio-quality 0 --extract-audio --audio-format m4a -o '~/Music/%(title)s.%(ext)s' " link)))

(progn
  (define-prefix-command 'mpc-keymap)
  (define-key mpc-keymap (kbd "t") 'mpc-toggle)
  (define-key mpc-keymap (kbd "n") 'mpc-next)
  (define-key mpc-keymap (kbd "p") 'mpc-previous)
  (define-key mpc-keymap (kbd "c") 'mpc-clear)
  (define-key mpc-keymap (kbd "u") 'mpc-update)
  (define-key mpc-keymap (kbd "d") 'mpc-delete)
  (define-key mpc-keymap (kbd "r") 'mpc-repeat)
  (define-key mpc-keymap (kbd "s") 'mpc-single)
  (define-key mpc-keymap (kbd "f") 'mpc-skip-forward)
  (define-key mpc-keymap (kbd "b") 'mpc-skip-backward)
  (define-key mpc-keymap (kbd "a s") 'mpc-add-song)
  (define-key mpc-keymap (kbd "a a") 'mpc-add-artist)
  (define-key mpc-keymap (kbd "l") 'mpc-download)
  (global-set-key (kbd "C-c m") 'mpc-keymap))

(defun cam-record-start ()
  (interactive)
  (async-shell-command "mpv av://v4l2:/dev/video0 --profile=low-latency --untimed"))

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
   "Wallpaper" nil "feh --bg-scale --randomize ~/Pictures/*"))

(defun wallpaper-toggle-cycle ()
  (interactive)
  (run-with-timer 0 20 #'wallpaper-set-wallpaper))

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

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))

(progn
  (define-prefix-command 'whitespace-keymap)
  (define-key whitespace-keymap (kbd "t") 'global-whitespace-mode)
  (global-set-key (kbd "C-c w") 'whitespace-keymap))

;; (defun checksum ()
;;   (interactive)
;;   (let ((choices '("md5" "sha1" "sha224" "sha256" "sha384" "sha512")))
;;     (setq x (ido-completing-read "Choose algorithm hash: " choices))
;;     (message "Result: %s"
;; 	     (secure-hash
;; 	      x
;; 	      (read-file-name "Enter file name: ")))))

(find-file "~/Personal/Org/Browser/Bookmarks.org")
(org-agenda-list)
