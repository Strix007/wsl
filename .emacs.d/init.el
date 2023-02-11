;; Enable server mode (daemon) for this Emacs session
;; (server-start)

;; Move customization variables to a separate file and load it
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror 'nomessage)

;; Create backup files in a seperate directory
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; Load nord theme
(defun load-nord-theme (frame)
  (select-frame frame)
  (load-theme 'nord t))
(if (daemonp)
	(add-hook 'after-make-frame-functions #'load-nord-theme)
  (load-theme 'nord t))


;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)
;; Disable startup message
(setq inhibit-startup-message t)

;; Disable not needed UI elements
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)   ; Disable the menu bar

;; Keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-h") 'package-install)
(global-set-key (kbd "M-RET") 'shell)


;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
               term-mode-hook
               eshell-mode-hook
               shell-mode-hook
               term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font
(set-face-attribute 'default nil :font "JetBrains Mono" :height 125)
(defun efs/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 125)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 125)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :height 125 :weight 'regular))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
                (setq dashboard-set-heading-icons t)
                (setq dashboard-set-file-icons t)
                (with-selected-frame frame
                  (efs/set-font-faces))))
    (efs/set-font-faces))

;; Initialize package sources
(require 'package)
(setq package-archives '(
			                   ("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")
			 ))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Evil
(use-package evil
  :config (evil-mode 1))

;; Dashboard
(use-package dashboard
  :ensure t
  :init 
    (setq dashboard-set-footer nil)
    (setq dashboard-set-navigator t)
    (setq dashboard-center-content t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-init-info nil)
    (setq dashboard-set-heading-icons t)
    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
    (setq dashboard-banner-logo-title "Welcome, Arbab")
    (setq dashboard-startup-banner "/home/arbab/.emacs.d/ProfilePic.jpg")
    (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")))
  )

;; Ace-pop-up menu
(use-package ace-popup-menu
  :init 
    (setq ace-popup-menu-show-pane-header t)
  :config 
    (ace-popup-menu-mode 1)
  )

;; Sublimity
(use-package sublimity
  :config (sublimity-mode 1)
  )

;; Neotree
(use-package neotree
  :config (
    (add-to-list 'load-path "/some/path/neotree")
    )
  :bind (
    ("C-8" . neotree-toggle)
    )
  )

;; Emojify
(use-package emojify
  :hook (after-init . global-emojify-mode)
  )

;; Dirvish
(use-package dirvish
  :init (
    dirvish-override-dired-mode
  ))

;; Powerline
;; (use-package powerline
  ;; :config (powerline-center-evil-theme)
  ;; )
(use-package doom-modeline
  :ensure t
  :init  
        (setq doom-modeline-indent-info t) 
        (setq doom-modeline-minor-modes nil)
        (doom-modeline-mode 1)
  )

;; Ivy
(use-package ivy
  :ensure t
  :init (ivy-mode)
  :diminish ivy
  :bind (
    ("C-s"     . swiper)
    ("C-c C-r" . ivy-resume)
    ("<f6>"    . ivy-resume)
    ("M-x"     . counsel-M-x)
    ("C-x C-f" . counsel-find-file)
    ("<f1> f"  . counsel-describe-function)
    ("<f1> v"  . counsel-describe-variable)
    ("<f1> o"  . counsel-describe-symbol)
    ("<f1> l"  . counsel-find-library)
    ("<f2> i"  . counsel-info-lookup-symbol)
    ("<f2> u"  . counsel-unicode-char)
    ("C-c g"   . counsel-git)
    ("C-c j"   . counsel-git-grep)
    ("C-c k"   . counsel-ag)
    ("C-x l"   . counsel-locate)
    ("C-S-o"   . counsel-rhythmbox)
        )
    )

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; Which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-ide-delay 0)
  )