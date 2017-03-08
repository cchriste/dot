;; Add my emacs libs to load path
(setq load-path (cons (substitute-in-file-name "$HOME/dot/emacs") load-path))

(require 'palette)
(require 'doremi-frm)

;; default colors
;;(set-background-color "gray95")
;;(set-background-color "#fffce5")   ;;light yellow
;;(set-background-color "#e5fcff") ;;light blue
;;(set-background-color "#CC6AEEEECC6A")   ;;light green
(set-background-color "#404552C54045")   ;;dark gray-green

;(set-foreground-color "gray20")
(set-foreground-color "snow")
(set-cursor-color "orange")
;(load-theme 'solarized-light-theme t)

;;codesearch
(require 'codesearch)
(setq codesearch-csearchindex (getenv "CSEARCHINDEX"))

;; don't ignore (ViSUS) .idx files for completion
(setq completion-ignored-extensions (delete ".idx" completion-ignored-extensions))

;; MATLAB (OSX)
;; check OS type
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (message "Mac OS X")
    (setq load-path (cons (substitute-in-file-name "$HOME/dot/emacs/matlab") load-path))
    (load-library "matlab-load")
    (matlab-cedet-setup)
    ;;(setq matlab-shell-command-switches '("-nojvm"))
    ;;(setq matlab-shell-command-switches '("-nosplash"))
    ))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux"))))

;; fix drag-and-drop in osx
(define-key global-map [ns-drag-file] 'my-ns-open-files)
(defun my-ns-open-files ()
  "Open files in the list `ns-input-file'."
  (interactive)
  (mapc 'find-file ns-input-file)
  (setq ns-input-file nil))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq tab-stop-list (number-sequence 4 200 4))

(setq default-tab-width 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-offsets-alist (quote ((arglist-intro . +) (arglist-cont c-lineup-gcc-asm-reg 0))))
 '(compilation-skip-threshold 2)
 '(desktop-path (quote ("." "~/.emacs.d/" "~")))
 '(desktop-restore-eager 7)
 '(desktop-save-mode t)
 '(display-time-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(fill-column 80)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(gud-tooltip-mode t)
 '(icomplete-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(large-file-warning-threshold nil)
 '(line-move-visual nil)
 '(list-command-history-max nil)
 '(msb-mode t)
 '(ns-alternate-modifier (quote alt))
 '(ns-command-modifier (quote meta))
 '(ns-right-alternate-modifier (quote super))
 '(nxml-sexp-element-flag t)
 '(partial-completion-mode nil)
 '(safe-local-variable-values (quote ((TeX-master . "dynamic_remote_analysis"))))
 '(savehist-mode t nil (savehist))
 '(scroll-down-aggressively 0.1)
 '(scroll-up-aggressively 0.1)
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(standard-indent 2)
 '(tab-always-indent (quote complete))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.emacs.d/autosave"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((t (:background "purple" :foreground "white" :weight bold))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :weight bold))))
 '(font-lock-comment-face ((t (:foreground "light cyan"))))
 '(font-lock-constant-face ((t (:foreground "cyan1" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "green" :weight bold))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(font-lock-string-face ((t (:foreground "light goldenrod"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow")))))
(put 'scroll-left 'disabled nil)

;;; Insert current date (from http://www.emacswiki.org/emacs/InsertDate)
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y.%m.%d.%H%M")
                   ((equal prefix '(4)) "%d.%m.%Y")
                   ((equal prefix '(16)) "%A, %B %d, %Y"))))
      (insert (format-time-string format))))

(defun insert-standard-date ()
    "Inserts standard date time string." 
    (interactive)
    (insert (format-time-string "%c")))

;;; Revert the current buffer without asking first.
(defun fast-revert-buffer ()
"Revert the buffer from the original file without asking."
  (interactive)
  (revert-buffer t t))

;;; Assign shortcuts
(global-set-key "\C-ce" 'ediff-buffers)
(global-set-key (kbd "C-c l") 'insert-date)
(global-set-key (kbd "C-S-p") '(lambda() (interactive) (scroll-down 1)))
(global-set-key (kbd "C-S-n") '(lambda() (interactive) (scroll-up 1)))
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-cr" 'fast-revert-buffer)
(global-set-key "\C-ct" 'toggle-truncate-lines)
(global-set-key "\C-cd" 'hide-subtree)
(global-set-key "\C-cs" 'show-subtree)
(global-set-key "\C-x\M-\." 'tags-search)
(global-set-key "\C-x\M->" 'tags-query-replace)
(global-set-key "\C-c\M-q" 'align-regexp)
(global-set-key "\C-c\C-v" 'codesearch-search-at-point)
(global-set-key "\C-c\C-f" 'codesearch-search)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-k" 'compile)
(global-set-key (kbd "C-;") 'indent-relative)

(c-add-style "VisIt" '("gnu" 
                       (c-basic-offset . 4)
                       (c-offsets-alist . ((innamespace . 0)
                                           (substatement-open 0)
                                           (brace-list-open 0)
                                           (access-label . -)
                                           (case-label . 0)
                                           (member-init-intro . +)
                                           (topmost-intro . 0)))))

(c-add-style "ir-tools" '("gnu" 
                          (c-basic-offset . 4)
                          (c-offsets-alist . ((innamespace . 0)
                                              (substatement-open 0)
                                              (brace-list-open 0)
                                              (access-label . -)
                                              (case-label . 0)
                                              (member-init-intro . +)
                                              (topmost-intro . 0)))))

(c-add-style "ViSUS" '("gnu" 
                       (c-basic-offset . 2)
                       (tab-width . 2)
                       (c-require-final-newline . 'nil)
                       (fill-column . 95)
                       (c-offsets-alist . ((innamespace . 0)
                                           (substatement-open 0)
                                           (statement-cont 0)
                                           (brace-list-open 0)
                                           (access-label . -)
                                           (case-label . 0)
                                           (member-init-intro . +)
                                           (case-label . +)
                                           (statement-case-open . 0)
                                           (inextern-lang . 0)
                                           (topmost-intro . 0)))))

(c-add-style "SCHOOL" '("ViSUS" 
                       (c-basic-offset . 2)))

(c-add-style "PIDX" '("ViSUS" 
                       (c-basic-offset . 2)))

(c-add-style "UINTAH" '("ViSUS" 
                       (c-basic-offset . 2)))

;; pick default style based on environment var 'MYPROJECT'
(setq CurrentProject (getenv "MYPROJECT"))
(setq my-c-style "ViSUS")
(if (equal CurrentProject "VISUS")
    (setq my-c-style "ViSUS")
  (if (equal CurrentProject "VISIT")
      (setq my-c-style "VisIt")
    (if (equal CurrentProject "PIDX")
        (setq my-c-style "PIDX")
      (if (equal CurrentProject "UINTAH")
          (setq my-c-style "UINTAH")
        (if (equal CurrentProject "SCHOOL")
            (setq my-c-style "SCHOOL")
          (if (equal CurrentProject "RT")
              (setq my-c-style "SCHOOL")
            (if (equal CurrentProject "HWRT")
                (setq my-c-style "SCHOOL")
              (if (equal CurrentProject "IRTOOLS")
                  (setq my-c-style "ir-tools")
                (setq my-c-style "ViSUS")))))))))

;;
;;C++ mode
;;
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.vert" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.frag" . c++-mode))

;;C++ mode hooks
;; <ctc> still need:
;;        X no indent after namespace and in switch/case
;;        X drag and drop should open new file, not copy contents of file into current buffer
;;        o y_or_n instead of requiring yes_or_no
;;        X a way to set c-basic-offset to 4 for some projects (VisIt)
;;        

;;; Save c-basic-offset and c-offsets-alist in .emacs.desktop
(add-to-list 'desktop-globals-to-save 'c-basic-offset)
(add-to-list 'desktop-globals-to-save 'c-offsets-alist)

;; rst-mode
(add-to-list 'auto-mode-alist '("\\.md" . rst-mode))

;; Discovered macros to use with:
;; (read-kbd-macro "C-c <left>")
;; You can use view-lossage to see what to put in the quotes.
(defun hs-mode-set-keys ()
	(progn
    (message "running hs-mode-set-keys hook...")
		(local-set-key [3 right] 'hs-show-block)
		(local-set-key [3 left] 'hs-hide-block)
		(local-set-key [3 up] 'hs-hide-level)
		(local-set-key [3 down] 'hs-show-all)
))
(add-hook 'hs-minor-mode-hook 'hs-mode-set-keys)

(add-hook 'c-mode-common-hook 'hs-minor-mode)
;;(add-hook 'c-mode-common-hook 'outline-minor-mode)

;;Find other file
(defun ff-find-other-file-set-keys()
  (progn
    (message "running ff-find-other-file-set-keys hook...")
    (local-set-key (kbd "C-c a") 'ff-find-other-file)))
(add-hook 'c-mode-common-hook 'ff-find-other-file-set-keys)
	    
(add-hook 'c-mode-common-hook (lambda() (local-set-key "\M-j" 'indent-for-comment)))

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (message "running my-c-mode-common-hook...")
  (c-set-style my-c-style))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Python Hook
(add-hook 'python-mode-hook
          'my-python-customizations)
(defun my-python-customizations ()
  "set up my personal customizations for python mode"
  (setq python-indent 4)
  (local-unset-key (kbd "C-c C-c"))
  (local-set-key "\C-c\C-c" 'comment-region))

;;Shell scripts
(defun cam-setup-sh-mode ()
  "My own personal preferences for `sh-mode'.

This is a custom function that sets up the parameters I usually
prefer for `sh-mode'.  It is automatically added to
`sh-mode-hook', but is can also be called interactively."
  (interactive)
  (setq sh-basic-offset 2
        sh-indentation 2
        sh-indent-after-function 2))
(add-hook 'sh-mode-hook 'cam-setup-sh-mode)

;; vc-ediff
(require 'vc-ediff)

(defun my-vc-dir-hide-up-to-date ()
  (interactive)
  (vc-dir-hide-up-to-date)
  (vc-dir-hide-unregistered))

(defun vc-dir-hide-unregistered ()
  "Hide unregistered items from display."
  (interactive)
  (let ((crt (ewoc-nth vc-ewoc -1))
        (first (ewoc-nth vc-ewoc 0)))
    ;; Go over from the last item to the first and remove the
    ;; unregistered files and directories with no child files.
    (while (not (eq crt first))
      (let* ((data (ewoc-data crt))
             (dir (vc-dir-fileinfo->directory data))
             (next (ewoc-next vc-ewoc crt))
             (prev (ewoc-prev vc-ewoc crt))
             ;; ewoc-delete does not work without this...
             (inhibit-read-only t))
        (when (or
               ;; Remove directories with no child files.
               (and dir
                    (or
                     ;; Nothing follows this directory.
                     (not next)
                     ;; Next item is a directory.
                     (vc-dir-fileinfo->directory (ewoc-data next))))
               ;; Remove files in the unregistered state.
               (eq (vc-dir-fileinfo->state data) 'unregistered))
          (ewoc-delete vc-ewoc crt))
        (setq crt prev)))))


(defun my-ediff-revision (file rev1 &optional rev2)
  "Run Ediff by comparing 'master' against the 'current'."
  (find-file file)
  (if (and (buffer-modified-p)
           (y-or-n-p (format "Buffer %s is modified. Save buffer? "
                             (buffer-name))))
      (save-buffer (current-buffer)))

  (ediff-load-version-control)
  (ediff-vc-internal rev1 rev2 nil))

(defun my-vc-diff (&optional arg)
  (interactive "P")
  (call-interactively
   (cond (arg (lambda nil (interactive) (vc-diff nil)))
         (t (lambda nil (interactive)
              (my-ediff-revision (buffer-file-name)
                                 (read-string "revision? " "HEAD" nil "HEAD")
                                 ""))))))

(add-hook  'vc-dir-mode-hook
           (lambda nil
             (define-key vc-dir-mode-map "x" 'my-vc-dir-hide-up-to-date)
             (define-key vc-dir-mode-map "E" 'my-vc-diff)))


;;; Remove ^Ms from the entire buffer.
(defun strip-control-ms ()
  "Delete all ^Ms."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (and (not (eobp))
		(re-search-forward "\C-m" nil t))
      (delete-region (match-beginning 0) (match-end 0)))))


;;ctc - get machine name, 11 for desktop, 10 for laptop
(if (or (string-prefix-p "jupiter" (system-name)) (string-prefix-p "mercury" (system-name)))
    (set-face-attribute 'default nil :font "-apple-Monaco-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-12:weight=Regular"))
    ;(set-face-attribute 'default nil :font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-fontset-auto2")
    ;(set-face-attribute 'default nil :font "-apple-Monaco-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1")))

(if (string-prefix-p "gunship" (system-name))
    (message "configuring for gunship...")
    (custom-set-variables
     '(x-meta-keysym (quote alt))
     '(x-super-keysym (quote meta))
     ))

(put 'upcase-region 'disabled nil)
