;; Enable server mode (daemon) for this Emacs session
;; (server-start)

;; Move customization variables to a separate file and load it
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror 'nomessage)
;; Change the location of the native compilation cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

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

;; No-littering
(use-package no-littering)
;; Move files that are saved when the edit in a buffer is saved to a direcotry under ~/.emacs.d
(setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Install doom-nord theme
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (setq doom-themes-padded-modeline nil)
  (doom-themes-neotree-config) 
  (doom-themes-org-config)
  (load-theme 'doom-nord t)
  )

;; Solaire
(use-package solaire-mode
  :init (solaire-global-mode +1)) 

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
		term-mode-hook
		neotree-mode-hook))
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
                (with-selected-frame frame
                  (efs/set-font-faces))))
  (efs/set-font-faces))

;; Evil
(use-package evil
  :config (evil-mode 1))

;; All-the-icons
(use-package all-the-icons
  :ensure t)

;; Projectile
(use-package projectile
  :config (projectile-mode +1)
  :bind 
  ("C-c p" . projectile-command-map)
  )

;; Dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info nil)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  ;; (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-banner-logo-title "Welcome, Arbab")
  (setq dashboard-startup-banner "/home/arbab/.emacs.d/banner.jpg")
  (setq dashboard-items '(
                          (recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          ;;  (agenda . 5)
                          ;;  (registers . 5)
                          ))
  (setq dashboard-item-names '(
                               ;;  ("Recent Files:" . " Recent Files:")
                               ;;  ("Bookmarks:" . " Bookmarks:")
                               ;;  ("Projects:" . " Projects:")
                               ))
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
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
              (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
              (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
              (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
  :config 
  (add-to-list 'load-path "/some/path/neotree")
  :bind (
	 ("<f8>" . neotree-toggle)
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

(use-package helpful
  :custom
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-h f" . helpful-function )
  ("C-h c" . helpful-command)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  )
;; Ivy
(use-package counsel
  :ensure t
  :init (ivy-mode)
  :diminish ivy
  :bind (
	 ("C-s"     . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("<f6>"    . ivy-resume)
	 ("M-x"     . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x b"   . counsel-ibuffer)
	 ("<f1> l"  . counsel-find-library)
	 ("<f2> i"  . counsel-info-lookup-symbol)
	 ("<f2> u"  . counsel-unicode-char)
	 ("C-c g"   . counsel-git)
	 ("C-c j"   . counsel-git-grep)
	 ("C-c k"   . counsel-ag)
	 ("C-x l"   . counsel-locate)
         )
  )

;; Ivy-rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  )

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; Which-key
(use-package which-key
  :init
  (which-key-mode)
  (which-key-setup-side-window-right)
  :diminish which-key-mode
  :config
  (setq which-key-ide-delay 0)
  )

;; Counsel-spotify
(use-package counsel-spotify
  :init
  (setq counsel-spotify-client-id "021a3ba7d3084e0fab36b2c7ea07d536")
  (setq counsel-spotify-client-secret "b0ff80fc4db14384acd46888485d6945")
  :bind (
	 ("C-c s s" . counsel-spotify-toggle-play-pause) 
	 ("C-c s a" . counsel-spotify-search-album) 
	 ("C-c s d" . counsel-spotify-search-artist) 
	 ("C-c s t" . counsel-spotify-search-track) 
	 ("C-c s v" . counsel-spotify-search-playlist) 
	 )
  )

(use-package emmet-mode
  :init (emmet-mode)
  )
