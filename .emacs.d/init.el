;; Fonts
(set-face-attribute 'default nil        :font "JetBrains Mono" :height 125 :weight 'medium)
(set-face-attribute 'fixed-pitch nil    :font "JetBrains Mono" :height 150 :weight 'medium)
(set-face-attribute 'variable-pitch nil :font "Cantarell"      :height 150 :weight 'bold)
;; Make comments italic
(add-hook 'find-file-hook (lambda () (set-face-attribute 'font-lock-comment-face nil :slant 'italic)))
;; Make keywords italic
(add-hook 'find-file-hook (lambda () (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)))

;; Initialize package sources
(require 'package)
(setq package-archives
      '(
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("elpa"  . "https://elpa.gnu.org/packages/")
        )
      )

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents)
  )

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  )

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)

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

;; High contrast nord theme
;; https://git.sr.ht/~ashton314/nordic-night
;; (load-file "~/.emacs.d/nordic-night-theme.el")
;; (load-theme 'nordic-night t)

;; Dashboard
(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :config
  (dashboard-modify-heading-icons '((recents . "file-text")))
  ;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-set-footer t)
  (setq dashboard-footer-messages '("Personal configuration of Arbab Khan"))
  (setq dashboard-footer-icon (all-the-icons-material "person"
                                                      :height 1.0
                                                      :v-adjust 0.0
                                                      :face 'font-lock-keyword-face))
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info "Welcome, Arbab")
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-banner-logo-title " ")
  (setq dashboard-startup-banner (concat user-emacs-directory "banner.jpg"))
  (setq dashboard-items '(
                          ;; (recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          ;; (agenda . 5)
                          ;; (registers . 5)
                          )
        )
  (setq dashboard-item-names '(
                               ;; ("Recent Files:" . " Recent Files:")
                               ;; ("Bookmarks:" . " Bookmarks:")
                               ;; ("Projects:" . " Projects:")
                               ("Recent Files:" . "Recent Files▾")
                               ("Bookmarks:" . "Bookmarks▾")
                               ("Projects:" . "Projects▾")
                               ("Agenda for the coming week:" . "Agenda▾")
                               ("Registers:" . "Registers▾")
                               )
        )
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `(
          (
           (
            ,(all-the-icons-octicon "mark-github" :height 1.0 :v-adjust 0.0)
            "Homepage"
            "Browse My Github Profile"
            (lambda (&rest _) (browse-url "https://github.com/Strix007"))
            font-lock-constant-face
            )
           (" "
            "Configuration"
            "Open Configuration"
            (lambda (&rest _) (find-file ".emacs.d/init.el"))
            warning
            )
           (
            ,(all-the-icons-material "restore" :height 1.0 :v-adjust 0.0)
            "Restore"
            "Restore Your Last Session"
            (lambda (&rest _)(desktop-read "~/"))
            error
            )
           )
          (
           (
            ,(all-the-icons-material "settings" :height 1.0 :v-adjust 0.0)
            "dotfiles"
            "View My Dotfiles"
            (lambda (&rest _) (browse-url "https://github.com/Strix007/dotfiles"))
            font-lock-function-name-face
            )
           )
          )
        )
  )

;; Solaire
(use-package solaire-mode
  :config
  (solaire-global-mode +1)
  )

;; Evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-respect-visual-line-mode t)
  (evil-mode 1)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-undo-system 'undo-tree)
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
  :after
  evil
  :config
  (evilnc-default-hotkeys)
  )

;; Evil-goggles
(use-package evil-goggles
  :after
  evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  )

;; Evil-snipe
(use-package evil-snipe
  :after
  evil
  :config
  (evil-snipe-override-mode 1)
  )

;; Evil-mc
(use-package evil-mc
  :after
  evil
  :config
  (global-evil-mc-mode 1)
  :bind
  ("C-M->" . evil-mc-make-cursor-in-visual-selection-end)
  ("C-M-<" . evil-mc-make-cursor-in-visual-selection-beg)
  ("C-M-/" . evil-mc-undo-all-cursors)
  )

;; All-the-icons
(use-package all-the-icons
  :config
  (when (and (not (member "all-the-icons" (font-family-list)))
             (window-system))
    (all-the-icons-install-fonts t)
    )
  )

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :custom
  (
   (projectile-completion-system 'ivy)
   )
  :config
  (setq projectile-project-search-path '("~/projects/"))
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  )

;; Counsel-projectile
(use-package counsel-projectile
  :after
  ivy
  projectile
  :config
  (counsel-projectile-mode)
  )

;; Ace-pop-up menu
(use-package ace-popup-menu
  :config
  (ace-popup-menu-mode 1)
  :config
  (setq ace-popup-menu-show-pane-header t)
  )

;; Neotree
(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  :bind
  ("<f8>" . neotree-toggle)
  )

;; Treemacs
(use-package treemacs
  :custom
  (treemacs-display-current-project-exclusively t)
  (treemacs-follow-mode t)
  :bind
  ("<f9>" . treemacs)
  )

;; Treemacs-evil
(use-package treemacs-evil
  :after
  treemacs
  )

;; Treemacs-projectile
(use-package treemacs-projectile
  :after
  treemacs
  )

;; Treemacs-magit
(use-package treemacs-magit
  :after
  treemacs
  )

;; Emojify
(use-package emojify
  :hook
  (after-init . global-emojify-mode)
  )

;; Dirvish
(use-package dirvish
  :init
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-find-file
    )
  (dirvish-override-dired-mode)
  :bind
  (
   ("C-x C-g" . dired-jump)
   :map dired-mode-map
   ("<return>" . dired-find-alternate-file)
   )
  :custom
  (dirvish-reuse-session nil)
  (delete-by-moving-to-trash t
        trash-directory "~/.local/share/Trash/files"
        )
  ;; Downloas "gls" and uncomment this line if you’re on OSX
  ;; (insert-directory-program "gls")
  )

;; All-the-icons-dired
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode)
  )

;; Dired-hide-dotfiles
(use-package dired-hide-dotfiles
  :after
  dirvish
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode)
  )

;; Minions
(use-package minions
  :hook
  (doom-modeline-mode . minions-mode)
  )
;; Doom-modeline
(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-mu4e t)
  (setq doom-modeline-irc t)
  (setq doom-modeline-height 30)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
  )

;; Helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-h f" . helpful-function)
  ("C-h c" . helpful-command)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  )

;; Ivy
(use-package counsel
  :diminish ivy
  :config
  (ivy-mode)
  (setq ivy-ignore-buffers '("\\` " "\\`\\*"))
  (setq ivy-height 15)
  (setq ivy-initial-inputs-alist nil)
  :bind
  (
   ("C-s"     . swiper)
   ("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x d"   . counsel-find-file)
   ("C-x b"   . counsel-switch-buffer)
   ("<f1> l"  . counsel-find-library)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ("<f2> u"  . counsel-unicode-char)
   ("C-c k"   . counsel-ag)
   ("C-x C-i" . counsel-imenu)
   :map ivy-minibuffer-map
   ("<tab>" . ivy-alt-done)
   ("M-<tab>" . ivy-immediate-done)
   ("M-k" . ivy-previous-line)
   ("M-j" . ivy-next-line)
   )
  )

;; All-the-icons-ivy-rich
(use-package all-the-icons-ivy-rich
  :after
  ivy
  :config
  (all-the-icons-ivy-rich-mode 1)
  )

;; Ivy-rich
(use-package ivy-rich
  :after
  ivy
  all-the-icons-ivy-rich
  :init
  (ivy-rich-mode 1)
  )

;; Ivy-posframe
(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-mode 1)
  )

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  )

;; Rainbow-mode
(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode)
  )

;; Which-key
(use-package which-key
  :defer t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 5)
  (which-key-mode)
  (which-key-setup-side-window-right)
  (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  (setq which-key-show-remaining-keys t)
  (setq which-key-allow-evil-operators t)
  (setq which-key-max-display-columns nil)
  )

;; Counsel-spotify
(use-package counsel-spotify
  :defer t
  :config
  (setq counsel-spotify-client-id spotify_class_id)
  (setq counsel-spotify-client-secret spotify_class_secret)
  )

;; Company
(use-package company
  :config
  (setq company-format-margin-function #'company-vscode-dark-icons-margin)
  :hook
  (prog-mode . company-mode)
  :custom
  (company-tooltip-limit 20)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :bind
  (
   ("M-<tab>" . company-complete)
   :map company-active-map
   ("M-<tab>" . company-complete-common)
   ("<tab>" . company-complete-selection)
   )
  )

;; Company-jedi
(use-package company-jedi
  :preface
  (defun arbab/python-company-setup ()
    (add-to-list 'company-backends 'company-jedi)
    )
  :hook
  (python-mode . arbab/python-company-setup)
  )

;; Company-quickhelp
(use-package company-quickhelp
  :hook
  (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-delay 0.0)
  )

;; General
(use-package general
  :config
  (general-auto-unbind-keys)
  ;; Define prefixes
  (general-create-definer arbab/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"
    )
  (general-create-definer arbab/avy
    :keymaps '(normal visual)
    :prefix "g SPC"
    )
  ;; Comment out when using centaur-tabs
  (general-define-key
   :states 'normal
   "gt" 'next-buffer
   "gT" 'previous-buffer
   )
  )
;; Define Keybindings
;; Define avy keybinding
(arbab/avy
  "e" '(avy-goto-char   :which-key "avy goto char")
  "E" '(avy-goto-char-2 :which-key "avy goto char-2")
  "w" '(avy-goto-word-1 :which-key "avy jump to word")
  "W" '(avy-goto-word-0 :which-key "avy word tree")
  "f" '(avy-goto-line   :which-key "avy goto line")
  "r" '(avy-resume      :which-key "avy resume")
  "l" '(link-hint-copy-link :which-key "copy link")
  "L" '(link-hint-open-link :which-key "open link")
  )
;; Define keys using space as leader
(arbab/leader-keys
  ;; Spotify keybinds using counsel-spotify
  "s"  '(:ignore t :which-key "Spotify")
  "ss" '(counsel-spotify-toggle-play-pause :which-key "Play-Pause")
  "sa" '(counsel-spotify-search-artist     :which-key "Search Artist")
  "sd" '(counsel-spotify-search-album      :which-key "Search Album")
  "sv" '(counsel-spotify-search-playlist   :which-key "Search Playlist")
  "st" '(counsel-spotify-search-track      :which-key "Search Track")
  ;; Increase or decrease text scale using hydra
  "t"  '(:ignore t :which-key "Text")
  "ts" '(hydra-text-scale/body :which-key "Scale")
  ;; Window Management
  ;; Manage Splits
  "x"  '(:ignore t :which-key "Window Management")
  "xw" '(hydra-splits/body  :which-key "Splits")
  "xh" '(split-window-right :which-key "Split Horizontally")
  "xv" '(split-window-below :which-key "Split Vertically")
  "xq" '(kill-this-buffer   :which-key "Kill Buffer")
  "xQ" '(centaur-tabs-kill-all-buffers-in-current-group :which-key "Kill All Buffers In Tab Group")
  "xb" '(counsel-switch-buffer :which-key "List Buffers")
  "xB" '(counsel-ibuffer :which-key "List All Buffers")
  "xc" '(delete-window      :which-key "Kill Split")
  "xC" '(delete-other-windows :which-key "Kill Splits Except Focused")
  "xf" '(ffap-other-window  :which-key "Open File In New Split")
  "xF" '(ffap-other-frame   :which-key "Open File In New Frame")
  ;; Navigate tabs using centaur-tabs
  "xj" '(centaur-tabs-backward-group :which-key "Move To Left Tab Group")
  "xk" '(centaur-tabs-forward-group  :which-key "Move To Right Tab Group")
  ;; Change theme
  "tt" '(load-theme :which-key "Load Theme")
  ;; Counsel Files
  "f"  '(:ignore t         :which-key "Files")
  "fr" '(counsel-recentf   :which-key "Recent Files")
  "ff" '(counsel-find-file :which-key "Find File")
  "fd" '(dired-jump        :which-key "Open Dired")
  "ft" '(ivy-resume        :which-key "Resume Ivy")
  ;; Bookmarks
  "b"  '(:ignore t        :which-key "Bookmark")
  "bb" '(counsel-bookmark :which-key "List Bookmarks")
  "bm" '(bookmark-set     :which-key "Add Bookmark")
  "br" '(bookmark-delete  :which-key "Remove Bookmark")
  )

;; Hydra
(use-package hydra
  :defer t
  )
;; Define a hydra for text scale
(defhydra hydra-text-scale (:color t)
  "Scale Text"
  ("=" text-scale-increase "Zoom In")
  ("-" text-scale-decrease "Zoom Out")
  ("ESC" nil "Finished" :exit t)
  )
;; Define a hydra for splits
(defhydra hydra-splits (:color t)
  "Manage Splits"
  ("[" shrink-window-horizontally  10 "Shrink Window Horizontally")
  ("]" enlarge-window-horizontally 10 "Enlarge Window Horizontally")
  ("-" shrink-window 10 "Shrink Window Vertically")
  ("=" balance-windows "Balance Windows")
  ("ESC" nil "Finished" :exit t)
  )

;; Magit
(use-package magit
  :commands
  (magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

;; Forge
(use-package forge
  :after
  magit
  )

;; Org-mode
(use-package org
  :commands
  (org-capture org-agenda)
  :preface
  (defun arbab/org-mode-setup ()
    (org-indent-mode)
    ;; (variable-pitch-mode 1)
    (visual-line-mode 1)
    (font-lock-add-keywords 'org-mode
                            '(
                              ("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")
                                         )
                                  )
                               )
                              )
                            )
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
                    )
                  )
      (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))
      )

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block           nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code            nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table           nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim        nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line       nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox        nil :inherit 'fixed-pitch)
    )
  :ensure org-contrib
  :hook
  (org-mode . arbab/org-mode-setup)
  :config
  (setq org-agenda-span 10)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-start-with-log-mode t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-ellipsis "▾")
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-startup-with-inline-images nil)
  (setq org-image-actual-width 600)
  (setq org-hide-emphasis-markers t)
  (setq org-link-descriptive t)
  (setq org-pretty-entities nil)
  (setq org-hidden-keywords nil)
  ;; Org-agenda files
  (setq org-agenda-files
        '(
          "~/.emacs.d/OrgFiles/Tasks.org"
          )
        )
  (setq org-todo-keywords
        '(
          (sequence
           "TODO(t)"
           "NEXT(n)"
           "|"
           "DONE(d!)"
           )
          (sequence
           "TODAY(r)"
           "BACKLOG(b)"
           "PLAN(p)"
           "READY(r)"
           "ACTIVE(a)"
           "URGENT(u)"
           "HOLD(h)"
           "|"
           "COMPLETED(c)"
           "CANC(k@)"
           )
          )
        )

  (setq org-refile-targets
        '(
          ("Archive.org" :maxlevel . 1)
          ("Tasks.org"   :maxlevel . 1)
          )
        )

  ;; Save Org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '(
          (:startgroup)
          ;; Put mutually exclusive tags here

          (:endgroup)
          ("@errand"  . ?E)
          ("@home"    . ?H)
          ("agenda"   . ?a)
          ("planning" . ?p)
          ("publish"  . ?P)
          ("batch"    . ?b)
          ("note"     . ?n)
          ("idea"     . ?i)
          ("goal"     . ?g)
          )
        )
  )

;; Org-super-agenda
(use-package org-super-agenda
  :after
  org
  :config
  (setq org-super-agenda-groups
        '(
          (
           :name "Today"
           :time-grid t
           :todo "TODAY"
           :order 0
           )
          (
           :name "Important"
           :todo "URGENT"
           :priority "A"
           :order 1
           )
          (
           :name "Active"
           :time-grid t
           :todo "ACTIVE"
           :order 2
           )
          (
           :name "Next Items"
           :todo "NEXT"
           :tag ("NEXT")
           :order 3
           )
          (
           :name "Errands"
           :tag ("errand")
           :order 4
           )
          (
           :name "Plans"
           :tag ("planning")
           :order 5
           )
          (
           :name "Ideas and Goals"
           :tag ("idea" "goal")
           :order 6
           )
          (
           :name "Notes"
           :tag ("note")
           :order 7
           )
          (
           :name "Quick Picks"
           :effort< "0:30"
           :order 1
           )
          (
           :priority<= "B"
           :scheduled future
           :order 1
           )
          )
        )
  :hook
  (org-agenda-mode . org-super-agenda-mode)
  )

;; Org-wild-notifier
(use-package org-wild-notifier
  :after
  org
  :config
  (org-wild-notifier-mode 1)
  :custom
  (org-wild-notifier-alert-time '(1 15 30 60))
  (org-wild-notifier-notification-tile "Org Agenda")
  (org-wild-notifier-notification-icon "")
  (org-wild-notifier-keyword-whitelist `("TODO" "NEXT"))
  )

;; Org-modern
(use-package org-modern
  :after
  org
  :hook
  (org-mode . org-modern-mode)
  :config
  (setq
   org-modern-star '("◉" "○" "●" "○" "●" "○" "●")
   org-modern-list '((42 . "") (43 . "") (45 . ""))
   org-modern-checkbox nil
   org-modern-hide-stars nil
   org-modern-tag t
   org-modern-priority t
   org-modern-todo t
   org-modern-table nil
   org-modern-priority t
   org-modern-block-name t
   org-modern-block-fringe 0
   org-modern-keyword t
   org-modern-statistics t
   org-modern-progress '("○" "◔" "◐" "◕" "●")
   )
  )

;; Org-appear
(use-package org-appear
  :after
  org
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t)
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers nil)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords nil)
  (setq org-appear-delay 0)
  )

;; Org-roam
(use-package org-roam
  :after
  org
  :config
  (setq org-roam-v2-ack t)
  (org-roam-setup)
  :custom
  (org-roam-directory "~/Notes")
  :bind
  (
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("M-<tab>"    . completion-at-point)
   )
  )

;; Toc-org
(use-package toc-org
  :after
  org
  :hook
  (org-mode . toc-org-mode)
  )

;; Org-babel-templates
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh"  . "src shell"))
(add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py"  . "src python"))
(add-to-list 'org-structure-template-alist '("lua" . "src lua"))

;; Visual-fill-column
(use-package visual-fill-column
  :commands
  (visual-fill-column-mode global-visual-fill-column-mode)
  :preface
  (defun arbab/org-mode-visual-fill ()
    (setq visual-fill-column-width 150)
    (visual-fill-column-mode 1)
    )
  :config
  (setq-default visual-fill-column-center-text t)
  ;; :hook
  ;; (org-mode . arbab/org-mode-visual-fill)
  )

;; Haskell-mode
(use-package haskell-mode
  :mode "\\.hs\\’"
  )

;; Lua-mode
(use-package lua-mode
  :mode "\\.lua\\’"
  )

;; Rust-mode
(use-package rust-mode
  :mode "\\.rs\\’"
  )

;; Json-mode
(use-package json-mode
  :mode "\\.json\\’"
  )

;; Typescript-mode
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  )

;; Tldr
(use-package tldr
  :defer t
  )

;; Vterm
(use-package vterm
  :bind
  ("M-RET" . vterm)
  :custom
  (vterm-max-scrollback 10000)
  )

;; Markdown-preview-eww
(use-package markdown-preview-eww)

;; Make sure to install the language servers on your local machine
;; LSP
;; LSP-mode
(use-package lsp-mode
  :preface
  (defun arbab/lsp-mode-setup ()
    ;; Run "lsp-deferred" if it's a supported mode
    (unless (derived-mode-p
             'emacs-lisp-mode
             'yuck-mode
             )
      (lsp-deferred)
      (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
      (lsp-headerline-breadcrumb-mode)
      )
    )
  :commands (lsp lsp-deferred)
  :hook
  (prog-mode . arbab/lsp-mode-setup)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-which-key-integration t)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (lsp-lens-enable nil)
  )

;; Lsp-ivy
(use-package lsp-ivy
  :after
  lsp-mode
  )

;; LSP-UI
(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 0.0)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-enhanced-markdown t)
  (lsp-ui-doc-use-childframe t)
  )

;; LSP-Haskell
(use-package lsp-haskell
  :after
  lsp-mode
  )

;; LSP-pyright
(use-package lsp-pyright
  :after
  lsp-mode
  )

;; LSP-treemacs
(use-package lsp-treemacs
  :after
  lsp-mode
  )

;; Ws-butler
(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode)
  )

;; Centaur-tabs
(use-package centaur-tabs
  :disabled t
  :init
  (setq centaur-tabs-set-bar 'left)
  (setq centaur-tabs-style "zigzag")
  (setq centaur-tabs-cycle-scope 'default)
  :hook
  (
   (
    dired-mode
    dashboard-mode
    vterm-mode
    tetris-mode
    quickrun--mode
    browse-kill-ring-mode
    special-mode
    tldr-mode
    compilation-mode
    ) . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-height 40)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-close-button "")
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "")
  (setq centaur-tabs-show-new-tab-button t)
  (setq centaur-tabs-new-tab-text "  ")
  (centaur-tabs-change-fonts "JetBrains Mono" 125)
  :bind
  (
   :map evil-normal-state-map
   ("gt" . centaur-tabs-forward)
   ("gT" . centaur-tabs-backward)
   )
  )

;; Yuck-mode
(use-package yuck-mode
  :mode "\\.yuck\\'"
  )

;; Parinfer-rust-mode
(use-package parinfer-rust-mode
  :defer t
  :config
  (setq parinfer-rust-auto-download t)
  )

;; Emmet
(use-package emmet-mode
  :hook
  (html-mode . emmet-mode)
  )

;; Undo-tree
(use-package undo-tree
  :init
  (setq undo-limit 10000000)
  (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist ("." . "~/.emacs.d/var/undo-tree-hist"))
  )

;; Try
(use-package try
  :commands
  (try)
  )

;; Sudo-edit
(use-package sudo-edit
  :bind
  ("C-c C-w" . sudo-edit)
  )

;; Beacon
(use-package beacon
  :init
  (beacon-mode 1)
  :custom
  (beacon-color "#5e81ac")
  )


;; Quickrun
(use-package quickrun
  :custom
  (quickrun-timeout-seconds nil)
  :bind
  ("C-c r r" . quickrun)
  ("C-c r w" . quickrun-region)
  ("C-c r e" . quickrun-replace-region)
  )

;; Corral
(use-package corral
  :config
  (setq corral-preserve-point t)
  :bind
  ("M-9" . corral-parentheses-backward)
  ("M-0" . corral-parentheses-forward)
  ("M-[" . corral-brackets-backward)
  ("M-]" . corral-brackets-forward)
  ("M-{" . corral-braces-backward)
  ("M-}" . corral-braces-forward)
  ("M-/" . corral-double-quotes-forward)
  ("M-1" . corral-backquote-forward)
  )

;; Nov
(use-package nov
  :preface
  (defun arbab/nov-mode-setup ()
    (face-remap-add-relative 'variable-pitch :family "Cantarell" :height 1.0)
    (visual-fill-column-mode)
    (visual-line-mode)
    )
  :config
  (setq nov-variable-pitch t)
  :mode
  ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode . arbab/nov-mode-setup)
  )

;; Vimish-fold
(use-package vimish-fold
  :after
  evil
  )

;; Evil-vimish-fold
(use-package evil-vimish-fold
  :after
  vimish-fold
  :init
  (global-evil-vimish-fold-mode)
  :config
  (setq evil-vimish-fold-mode-lighter "")
  (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  )

;; Save-visited-files
(use-package workgroups2
  :defer t
  :config
  (setq wg-prefix-key "C-c z")
  (setq wg-session-file (concat user-emacs-directory ".workgroups"))
  (workgroups-mode 1)
  )

;; Highlight-indentation
(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top)
  )

;; Sxhkd-mode
(use-package sxhkdrc-mode
  :config
  (setq sxhkd-mode-reload-config t)
  :mode
  ("\\sxhkdrc\\’" . sxhkdrc-mode)
  )

;; Minimap
(use-package minimap
  :commands
  (minimap-mode)
  :config
  (setq minimap-window-location 'right)
  (setq minimap-minimum-width 10)
  (setq minimap-dedicated-window t)
  (setq minimap-hide-cursor t)
  (setq minimap-hide-scroll-bar nil)
  (setq minimap-hide-fringes t)
  )

;; Hl-mode
(use-package hl-mode
  :ensure nil
  :hook
  (
   (
    prog-mode
    text-mode
    ) . hl-line-mode)
  )

;; Smartparens
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  )

;; Prescient
(use-package prescient
  :config
  (setq prescient-persist-mode t)
  (setq prescient-sort-length-enable nil)
  )

;; Ivy-prescient
(use-package ivy-prescient
  :after
  counsel
  :init
  (ivy-prescient-mode 1)
  :config
  (setq ivy-prescient-retain-classic-highlighting t)
  )

;; Company-prescient
(use-package company-prescient
  :after
  company
  :init
  (company-prescient-mode 1)
  )

;; Smex
(use-package smex)

;; Tree-sitter
(use-package tree-sitter
  :preface
  (defun arbab/tree-sitter-mode-setup ()
    (unless (derived-mode-p
             'emacs-lisp-mode
             'yuck-mode
             )
      (tree-sitter-hl-mode)
      )
    )
  :hook
  (prog-mode . arbab/tree-sitter-mode-setup)
  )

;; Tree-sitter-langs
(use-package tree-sitter-langs
  :after
  tree-sitter
  )

;; Git-modes
(use-package git-modes
  :mode
  ("\\.gitattributes\\'" . gitattributes-mode)
  ("\\.gitconfig\\'"     . gitconfig-mode)
  ("\\.gitignore\\'"     . gitignore-mode)
  )

;; Vimrc-mode
(use-package vimrc-mode
  :mode "\\.vim\\'"
  )

;; Nix-mode
(use-package nix-mode
  :mode "\\.nix\\'"
  )

;; Diminish
(use-package diminish)

;; Prettify-symbols-mode
(use-package prettify-symbols-mode
  :ensure nil
  :hook
  (prog-mode . prettify-symbols-mode)
  )

(use-package highlight-numbers
  :hook
  (prog-mode . highlight-numbers-mode)
  )

;; Highlight-escape-sequences
(use-package highlight-escape-sequences
  :hook
  (prog-mode . hes-mode)
  )

;; Popup-kill-ring
(use-package popup-kill-ring
  :bind
  (
   ("M-y" . popup-kill-ring)
   :map popup-kill-ring-keymap
   ("M-j" . popup-kill-ring-next)
   ("M-k" . popup-kill-ring-previous)
   ("<escape>" . keyboard-quit)
   )
  )

;; Browse-kill-ring
(use-package browse-kill-ring
  :bind
  ("M-Y" . browse-kill-ring)
  :custom
  (browse-kill-ring-highlight-current-entry 'solid)
  )

;; Drag-sutff
(use-package drag-stuff
  :bind
  ("C-j" . drag-stuff-down)
  ("C-k" . drag-stuff-up)
  )

;; Smart-hungry-delete
(use-package smart-hungry-delete
  :init
  (smart-hungry-delete-add-default-hooks)
  :bind
   ([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
  (
	 ([remap delete-backward-char] . smart-hungry-delete-backward-char)
	 ([remap delete-char] . smart-hungry-delete-forward-char)
   )
  )

;; Highlight-parentheses
(use-package highlight-parentheses
  :hook
  (prog-mode . highlight-parentheses-mode)
  )

;; Avy
(use-package avy
  :defer t
  :custom
  (avy-all-windows t)
  )

;; Link-hint
(use-package link-hint)

;; Dumb-jump
(use-package dumb-jump
  :hook
  (xref-backend-functions . dumb-jump-xref-activate)
  )

;; Smart-jump
(use-package smart-jump
  :bind
  (
   ("M-." . smart-jump-go)
   ("M-," . smart-jump-back)
   ("M-?" . smart-jump-references)
   )
  )

;; Hide-mode-line
(use-package hide-mode-line
  :hook
  (
   (
    dired-mode
    fundamental-mode
    dashboard-mode
    vterm-mode
    tetris-mode
    quickrun--mode
    browse-kill-ring-mode
    special-mode
    tldr-mode
    compilation-mode
    messages-buffer-mode
    diff-mode
    ) . hide-mode-line-mode
   )
  )

;; Jinx
(use-package jinx
  :disabled t
  :hook
  (text-mode . jinx-mode)
  :bind
  (
   :map text-mode-map
   ([remap ispell-word] . jinx-correct)
   )
  )

;; Ace-window
(use-package ace-window
  :bind
  (
   :map evil-normal-state-map
   ("C-w" . ace-window)
   )
  )

;; Define-word
(use-package define-word
  :bind
  (
   ("C-h ," . define-word-at-point)
   )
  )

;; Centered-cursor-mode
(use-package centered-cursor-mode
  :config
  (global-centered-cursor-mode)
  )
