;; Enable server mode (daemon) for this Emacs session
(server-start)

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

;; Auto-package-update
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "20:00")
  )

;; Install doom-nord theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t) 
  (setq doom-themes-padded-modeline nil)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
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
;; Restore the last location of the cursor in a file
(save-place-mode 1)

;; Modify UI elements
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)   ; Disable the menu bar

;; Indentation
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

;; Keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Move point from window to window using Shift and the arrow keys
(windmove-default-keybindings)

;; Auto close brackets and quotes
(electric-pair-mode)
(electric-quote-mode)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(
		completion-list-mode-hook 
		org-mode-hook
    dired-mode-hook
		Info-mode-hook
		calendar-mode-hook
		org-agenda-mode-hook
		dashboard-mode-hook
		vterm-mode-hook
		compilation-mode-hook
		backtrace-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook
		term-mode-hook
		neotree-mode-hook
		which-key-mode
		helpful-mode-hook
    treemacs-mode-hook
		))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Fonts
(set-face-attribute 'default nil        :font "JetBrains Mono" :height 125)
(set-face-attribute 'fixed-pitch nil    :font "JetBrains Mono" :height 150)
(set-face-attribute 'variable-pitch nil :font "Cantarell"      :height 150)

;; Evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-undo-system 'undo-redo)
  )

;; Evil-collection
(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init)
  )

;; Evil-nerd-commenter
(use-package evil-nerd-commenter
  :init
  (evilnc-default-hotkeys))

;; All-the-icons
(use-package all-the-icons)

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-project-search-path '("~/projects/"))
  :custom (
	   (projectile-completion-system 'ivy)
	   )
  :config
  (projectile-mode +1)
  :bind 
  ("C-c p" . projectile-command-map)
  )

;; Counsel-projectile
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Dashboard
(use-package dashboard
  :init
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info nil)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-banner-logo-title "Welcome, Arbab")
  (setq dashboard-startup-banner "/home/arbab/.emacs.d/banner.jpg")
  (setq dashboard-items '(
                          (recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          ;; (registers . 5)
                          ))
  (setq dashboard-item-names '(
                               ;; ("Recent Files:" . " Recent Files:")
                               ;; ("Bookmarks:" . " Bookmarks:")
                               ;; ("Projects:" . " Projects:")
                               ("Recent Files:" . "Recent Files▾")
                               ("Bookmarks:" . "Bookmarks▾")
                               ("Projects:" . "Projects▾")
			                         ("Agenda for the coming week:" . "Agenda▾")
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

;; Neotree
(use-package neotree
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  :config 
  (add-to-list 'load-path "/some/path/neotree")
  :bind 
  ("<f8>" . neotree-toggle)
  )

;; Treemacs
(use-package treemacs
  :bind
  ("<f9>" . treemacs))

;; Treemacs-evil
(use-package treemacs-evil
  :after treemacs)

;; Treemacs-projectile
(use-package treemacs-projectile
  :after treemacs)

;; Emojify
(use-package emojify
  :hook (after-init . global-emojify-mode)
  )

;; Dirvish
(use-package dirvish
  :init
	(dirvish-override-dired-mode)
  (setq dired-mouse-drag-files t)                   
  (setq mouse-drag-and-drop-region-cross-program t) 
  (setq delete-by-moving-to-trash t)
  :bind
   ("C-x C-g" . dired-jump)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory)
	)

;; All-the-icons-dired
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Dired-hide-dotfiles
(use-package dired-hide-dotfiles
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; Powerline
;; (use-package powerline
;; :config (powerline-center-evil-theme))

;; Doom-modeline
(use-package doom-modeline
  :init  
  (setq doom-modeline-indent-info t) 
  (setq doom-modeline-minor-modes nil)
  (doom-modeline-mode 1)
  )

;; Helpful
(use-package helpful
  :custom
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-h f" . helpful-function)
  ("C-h c" . helpful-command)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  )

;; Ivy
(use-package counsel
  :init (ivy-mode)
  :diminish ivy
  :bind 
  ("C-s"     . swiper)
  ("C-c C-r" . ivy-resume)
  ("C-x r b" . counsel-bookmark)
  ("<f6>"    . ivy-resume)
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x b"   . counsel-switch-buffer)
  ("<f1> l"  . counsel-find-library)
  ("<f2> i"  . counsel-info-lookup-symbol)
  ("<f2> u"  . counsel-unicode-char)
  ("C-c g"   . counsel-git)
  ("C-c j"   . counsel-git-grep)
  ("C-c k"   . counsel-ag)
  ("C-x l"   . counsel-locate)
  ("C-x w"   . counsel-wmctrl)
  )

;; Ivy-rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  )

;; Ivy-posframe
(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-mode 1)
  )

;; Rainbow-delimiters
(use-package rainbow-delimiters
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
  )

;; Company
(use-package company
  :hook
  (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  )

;; Company-box
(use-package company-box
  :hook (company-mode . company-box-mode)
  :bind
   ("M-<tab>" . company-complete)
  )

;; General
(use-package general
  :init
  (general-auto-unbind-keys)
  :config
  (general-create-definer arbab/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  )
(arbab/leader-keys
  ;; Spotify keybinds using counsel-spotify
  "s"  '(:ignore t :which-key "Spotify")
  "ss" '(counsel-spotify-toggle-play-pause :which-key "Play-Pause")
  "sa" '(counsel-spotify-search-artist     :which-key "Search Artist")
  "sd" '(counsel-spotify-search-album      :which-key "Search Album")
  "sv" '(counsel-spotify-search-playlist   :which-key "Search Playlist")
  "st" '(counsel-spotify-search-track      :which-key "Search Track")
  ;; Increase or decrease text scale using hydra
  "t" '(:ignore t :which-key "Text")
  "ts" '(hydra-text-scale/body :which-key "Scale")
  ;; Navigate tabs using centaur-tabs
  "<left>"    '(centaur-tabs-backward-tab               :which-key "Move To Left Tab")
  "<right>"   '(centaur-tabs-forward-tab                :which-key "Move To Right Tab")
  "S-<right>" '(centaur-tabs-forward-group              :which-key "Move To Right Tab Group")
  "S-<left>"  '(centaur-tabs-backward-group             :which-key "Move To Left Tab Group")
  "<up>"      '(centaur-tabs--create-new-tab            :which-key "Create New Tab")
  "w"         '(centaur-tabs--kill-this-buffer-dont-ask :which-key "Kill Tab")
  )

;; Hydra
(use-package hydra)
(defhydra hydra-text-scale (:color t)
  "scale text"
  ("=" text-scale-increase "Zoom In")
  ("-" text-scale-decrease "Zoom Out")
  ("ESC" nil "Finished" :exit t)
  )

;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Forge
(use-package forge)

(defun arbab/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Set faces for heading levels
  (dolist (face '(
		              (org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)
		  ))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block           nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code            nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table           nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim        nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line       nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox        nil :inherit 'fixed-pitch)
  )

;; Org-mode
(use-package org
  :hook
  (org-mode . arbab/org-mode-setup)
  :config
  (setq org-confirm-babel-evaluate nil) 
  (setq org-ellipsis "▾")
  (setq org-log-done 'note)
  ;; Org-agenda files
  (setq org-agenda-files
        '(
          "~/.emacs.d/OrgFiles/Tasks.org"
        ))
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)
       ("goal" . ?g)
       ))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))
  )

;; Org-bullets
(use-package org-bullets
  :after
  org
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  )

;; Org-babel-templates
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh"  . "src shell"))
(add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py"  . "src python"))
(add-to-list 'org-structure-template-alist '("lua" . "src lua"))

;; Visual-fill-column
(defun arbab/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook (org-mode . arbab/org-mode-visual-fill))

;; Haskell-mode
(use-package haskell-mode)

;; Lua-mode
(use-package lua-mode)

;; Typescript-mode
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  )

;; Vterm
(use-package vterm
  :bind
  ("M-RET" . vterm)
  :config
  (setq vterm-max-scrollback 10000)
  )

;; Markdown-preview-eww
(use-package markdown-preview-eww)

;; LSP
(defun arbab/lsp-mode-setup ()
  ;; Run "lsp-deferred" if it's a supported mode
  (unless (derived-mode-p 'emacs-lisp-mode 'yuck-mode)
    (lsp-deferred)
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode)
    )
  )

;; Make sure to install the language servers on your local machine
;; LSP-Haskell
(use-package lsp-haskell)

;; LSP-pyright
(use-package lsp-pyright)

;; LSP-treemacs
(use-package lsp-treemacs)

;; LSP-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (prog-mode . arbab/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  )

;; LSP-UI
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui)
  :custom
  (lsp-ui-doc-position 'bottom)
  )

;; Ws-butler
(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode))

;; Multiple-cursors
(use-package multiple-cursors
  :bind
  ("C-M-/" . mc/edit-lines)
  ("C-M-<" . mc/mark-previous-like-this)
  ("C-M->" . mc/mark-next-like-this)
  )

;; Centaur-tabs
(use-package centaur-tabs
  :init
  (setq centaur-tabs-set-bar 'left)
  (setq centaur-tabs-style "zigzag")
  (setq centaur-tabs-cycle-scope 'default)
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  (vterm-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-height 40)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-close-button "")
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "")
  (setq centaur-tabs-show-new-tab-button t)
  ;; (setq centaur-tabs-new-tab-text "")
  (centaur-tabs-change-fonts "Jetbrains Mono" 125)
  )

;; Yuck-mode
(use-package yuck-mode)

;; Parinfer-rust-mode
(use-package parinfer-rust-mode
  :init
  (setq parinfer-rust-auto-download t)
  )

;; Emmet
(use-package emmet-mode
  :hook
  (html-mode . emmet-mode))
