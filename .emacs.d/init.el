;; Straight.el
;; Bootstrap Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate with use-package
(straight-use-package 'use-package)
;; Always use straight.el
(setq straight-use-package-by-default +1)

;; Set Fonts
(set-face-attribute 'default nil        :font "JetBrains Mono"  :height 125 :weight 'medium)
(set-face-attribute 'fixed-pitch nil    :font "JetBrains Mono"  :height 150 :weight 'medium)
(set-face-attribute 'variable-pitch nil :font "Source Sans Pro" :height 150 :weight 'medium)

;; Initialize package sources
(require 'package)
(setq package-archives
      '(
        ("melpa"  . "https://melpa.org/packages/")
        ("elpa"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        )
      )

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents)
  )

;; Initialize use-package-vc on non-Linux platforms
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package")
  )
(require 'vc-use-package)

;; Use-package
(setq use-package-always-ensure nil)
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
  :hook
  (server-after-make-frame . (lambda () (load-theme 'doom-nord-aurora t)))
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (setq doom-themes-padded-modeline nil)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  :init
  (load-theme 'doom-nord-aurora t)
  )

;; High contrast nord theme
;; https://git.sr.ht/~ashton314/nordic-night
;; (load-file "~/.emacs.d/nordic-night-theme.el")
;; (load-theme 'nordic-night t)

;; Dashboard
(use-package dashboard
  :init
  (setq dashboard-icon-type 'all-the-icons)
  (dashboard-setup-startup-hook)
  :config
  (dashboard-modify-heading-icons '((recents . "file-text")))
  (setq initial-buffer-choice (lambda ()
                                (get-buffer-create "*dashboard*")
                                (dashboard-refresh-buffer)))
  (setq dashboard-center-content t)
  (setq dashboard-set-footer t)
  ;; Icons
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-footer-messages '("Personal configuration of Arbab Khan"))
  (setq dashboard-footer-icon (
                               all-the-icons-material "person"
                               :height 1.0
                               :v-adjust 0.0
                               :face 'font-lock-keyword-face
                               )
        )
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info "Welcome, Arbab")
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-banner-logo-title " ")
  (setq dashboard-startup-banner (concat user-emacs-directory "banner.jpg"))
  ;; (setq dashboard-startup-banner (concat user-emacs-directory "banner.txt"))
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
           (
            ,(all-the-icons-octicon "settings" :height 1.0 :v-adjust 0.0)
            "Configuration"
            "Open Configuration"
            (lambda (&rest _) (find-file "~/.emacs.d/init.el"))
            warning
            )
           (
            ,(all-the-icons-material "restore" :height 1.0 :v-adjust 0.0)
            "Restore"
            "Restore Your Last Session"
            (lambda (&rest _)(burly-open-last-bookmark))
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
  (setq evil-undo-system 'undo-fu)
  (evil-mode +1)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-undo-system 'undo-fu)
  (evil-define-key '(normal insert) 'global
    (kbd "M-c") #'arbab/smart-flexing-at-point
    (kbd "M-l") #'arbab/smart-casing-at-point
    )
  )

;; Evil-collection
(use-package evil-collection
  :after
  (evil)
  :config
  (evil-collection-init)
  )

;; Evil-nerd-commenter
(use-package evil-nerd-commenter
  :after
  (evil)
  :config
  (evilnc-default-hotkeys)
  )

;; Evil-goggles
(use-package evil-goggles
  :after
  (evil)
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  )

;; Evil-snipe
(use-package evil-snipe
  :after
  (evil)
  :config
  (evil-snipe-override-mode +1)
  )

;; Evil-surround
(use-package evil-surround
  :after
  (evil)
  :config
  (global-evil-surround-mode +1)
  )

;; Evil-anzu
(use-package evil-anzu
  :after
  (evil)
  :config
  (global-anzu-mode +1)
  :bind
  (
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   ([remap query-replace] . anzu-query-replace)
   )
  )

;; Evil-mc
(use-package evil-mc
  :after
  (evil)
  :config
  (global-evil-mc-mode 1)
  (evil-define-key '(normal visual) 'global
    "g.m" #'evil-mc-make-all-cursors
    "g.u" #'evil-mc-undo-all-cursors
    "g.z" #'+evil/mc-toggle-cursors
    "g.c" #'+evil/mc-make-cursor-here
    "g.n" #'evil-mc-make-and-goto-next-cursor
    "g.p" #'evil-mc-make-and-goto-prev-cursor
    "g.N" #'evil-mc-make-and-goto-last-cursor
    "g.P" #'evil-mc-make-and-goto-first-cursor
    )
  (with-eval-after-load 'evil-mc
    (evil-define-key '(normal visual) evil-mc-key-map
      (kbd "M-j") #'evil-mc-make-and-goto-next-cursor
      (kbd "M-J") #'evil-mc-make-and-goto-last-cursor
      (kbd "M-k") #'evil-mc-make-and-goto-prev-cursor
      (kbd "M-K") #'evil-mc-make-and-goto-first-cursor
      )
    )
  )

;; All-the-icons
(use-package all-the-icons
  :config
  (when (and (not (member "all-the-icons" (font-family-list)))
             (window-system))
    (all-the-icons-install-fonts t)
    )
  )

;; Nerd-icons
;; Make sure to do M-x nerd-icons-install-fonts
(use-package nerd-icons)

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :custom
  (
   (projectile-completion-system 'ivy)
   )
  :config
  (projectile-mode +1)
  :custom
  (projectile-project-search-path '("~/projects/"))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  )

;; Counsel-projectile
(use-package counsel-projectile
  :after
  (:any counsel projectile)
  :config
  (counsel-projectile-mode)
  )
;; Ace-pop-up menu

(use-package ace-popup-menu
  :config
  (ace-popup-menu-mode +1)
  :custom
  (ace-popup-menu-show-pane-header t)
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
  :config
  (setq aw-ignored-buffers (delete 'treemacs-mode aw-ignored-buffers))
  :custom
  (treemacs-display-current-project-exclusively nil)
  (treemacs-project-follow-mode nil)
  (treemacs-follow-mode t)
  :bind
  (
   ("<f9>" . treemacs)
   )
  )

;; Treemacs-evil
(use-package treemacs-evil
  :after
  (treemacs)
  :bind
  (
   :map evil-treemacs-state-map
   ("C-w" . ace-window)
   )
  )

;; Treemacs-projectile
(use-package treemacs-projectile
  :disabled t
  :after
  (treemacs)
  )

;; Treemacs-magit
(use-package treemacs-magit
  :after
  (treemacs)
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
    "w" 'wdired-change-to-wdired-mode
    "h" 'dired-up-directory
    "l" 'dired-open-file ; use dired-find-file instead of dired-open.
    "m" 'dired-mark
    "t" 'dired-toggle-marks
    "u" 'dired-unmark
    "C" 'dired-do-copy
    "D" 'dired-do-delete
    "J" 'dired-goto-file
    "M" 'dired-do-chmod
    "O" 'dired-do-chown
    "P" 'dired-do-print
    "R" 'dired-do-rename
    "T" 'dired-do-touch
    "Y" 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
    "Z" 'dired-do-compress
    "+" 'dired-create-directory
    "-" 'dired-do-kill-lines
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
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :after
  (dirvish)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode)
  )

;; Media-progress-dirvish
(use-package media-progress-dirvish
  :after
  (dirvish)
  :config
  (media-progress-dirvish-setup)
  )

;; Dired-preview
(use-package dired-preview
  :commands
  (dired-preview-mode)
  ;; :hook
  ;; (dired-mode . dired-preview-mode)
  :custom
  (setq dired-preview-delay 0.0)
  )

;; Dired-open
(use-package dired-open
  :config
  (setq dired-open-extensions '(
                                ("gif" . "feh")
                                ("jpg" . "feh")
                                ("png" . "feh")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv")
                                )
        )
  )

;; Minions
(use-package minions
  :hook
  (doom-modeline-mode . minions-mode)
  )

;; Doom-modeline
(use-package doom-modeline
  :config
  (doom-modeline-mode +1)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-support-imenu t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e t)
  (doom-modeline-irc t)
  (doom-modeline-height 30)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  )

;; Helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-h f" . counsel-describe-function)
  ("C-h c" . counsel-describe-symbol)
  ("C-h v" . counsel-describe-variable)
  ("C-h k" . helpful-key)
  )

;; Ivy
(use-package counsel
  :init
  (ivy-mode)
  :custom
  (ivy-extra-directories nil)
  (ivy-use-virtual-buffers nil)
  (enable-recursive-minibuffers t)
  (ivy-ignore-buffers '("\\` " "\\`\\*"))
  (ivy-height 15)
  (ivy-initial-inputs-alist nil)
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
  (counsel)
  :config
  (all-the-icons-ivy-rich-mode +1)
  )

;; Ivy-rich
(use-package ivy-rich
  :after
  (counsel all-the-icons-ivy-rich)
  :config
  (ivy-rich-mode +1)
  :custom
  (ivy-rich-path-style 'abbrev)
  )

;; Ivy-posframe
(use-package ivy-posframe
  :config
  (ivy-posframe-mode +1)
  :custom
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  )

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  )

;; Rainbow-mode
(use-package rainbow-mode
  :hook
  (
   (
    prog-mode
    text-mode
    ) . rainbow-mode)
  )

;; Which-key
(use-package which-key
  :defer t
  :diminish
  which-key-mode
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right)
  :custom
  (which-key-idle-delay 5)
  (which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  (which-key-show-remaining-keys t)
  (which-key-allow-evil-operators t)
  (which-key-max-display-columns nil)
  )

;; Counsel-spotify
(use-package counsel-spotify
  :defer t
  :custom
  (counsel-spotify-client-id spotify_class_id)
  (counsel-spotify-client-secret spotify_class_secret)
  )

;; Company
(use-package company
  :hook
  (prog-mode . company-mode)
  :custom
  (company-format-margin-function #'company-vscode-dark-icons-margin)
  (company-tooltip-limit 20)
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0)
  :bind
  (
   ("M-<tab>" . company-complete)
   :map company-active-map
   ("M-<tab>" . company-complete-common)
   ("<tab>" . company-complete-selection)
   ("C-<tab>" . yas-expand)
   )
  )

;; Company-jedi
(use-package company-jedi
  :preface
  (defun arbab/python-company-setup ()
    (add-to-list 'company-backends 'company-jedi)
    )
  :hook
  (
   (
    python-mode
    python-ts-mode
    ) . arbab/python-company-setup
   )
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
   :states '(normal visual)
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
  "xxf" '(counsel-switch-buffer-other-window :which-key "Open Buffer In New Split")
  ;; Navigate tabs using centaur-tabs
  ;; "xj" '(centaur-tabs-backward-group :which-key "Move To Left Tab Group")
  ;; "xk" '(centaur-tabs-forward-group  :which-key "Move To Right Tab Group")
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
  ;; Burly
  "z"  '(:ignore t                :which-key "Burly")
  "zb" '(burly-open-bookmark      :which-key "Open Burly Bookmarks")
  "zB" '(burly-open-last-bookmark :which-key "Open Last Burly Bookmark")
  "zf" '(burly-bookmark-frames    :which-key "Burly Bookmark Frame")
  "zw" '(burly-bookmark-windows   :which-key "Burly Bookmark Windows")
  ;; Corral
  "gr"  '(:ignore t                     :which-key "Corral")
  "gr9" '(corral-parentheses-backward   :which-key "corral insert parentheses backward")
  "gr0" '(corral-parentheses-forward    :which-key "corral insert parentheses forward")
  "gr[" '(corral-brackets-backward      :which-key "corral insert brackets backward")
  "gr]" '(corral-brackets-forward       :which-key "corral insert brackets forward")
  "gr{" '(corral-braces-backward        :which-key "corral insert brackes backward")
  "gr}" '(corral-braces-forward         :which-key "corral insert brackes forward")
  "gr;" '(corral-double-quotes-backward :which-key "corral insert double quotes backward")
  "gr:" '(corral-single-quotes-backward :which-key "corral insert double quotes backward")
  ;; Harpoon
  "h"   '(:ignore t           :which-key "Harpoon")
  "hc"  '(harpoon-clear       :which-key "Clear harpoon marks")
  "hf"  '(harpoon-toggle-file :which-key "Open harpoon mark file")
  "ha"  '(harpoon-add-file    :which-key "Add File To Harpoon Mark")
  "h1"  '(harpoon-go-to-1     :which-key "Go To Harpoon Mark 1")
  "h2"  '(harpoon-go-to-2     :which-key "Go To Harpoon Mark 2")
  "h3"  '(harpoon-go-to-3     :which-key "Go To Harpoon Mark 3")
  "h4"  '(harpoon-go-to-4     :which-key "Go To Harpoon Mark 4")
  "h5"  '(harpoon-go-to-5     :which-key "Go To Harpoon Mark 5")
  "h6"  '(harpoon-go-to-6     :which-key "Go To Harpoon Mark 6")
  "h7"  '(harpoon-go-to-7     :which-key "Go To Harpoon Mark 7")
  "h8"  '(harpoon-go-to-8     :which-key "Go To Harpoon Mark 8")
  "h9"  '(harpoon-go-to-9     :which-key "Go To Harpoon Mark 9")
  "hdd" '(harpoon-delete-item :which-key "Harpoon delete item")
  "hd1" '(harpoon-delete-1    :which-key "Delete Harpoon Mark 1")
  "hd2" '(harpoon-delete-2    :which-key "Delete Harpoon Mark 2")
  "hd3" '(harpoon-delete-3    :which-key "Delete Harpoon Mark 3")
  "hd4" '(harpoon-delete-4    :which-key "Delete Harpoon Mark 4")
  "hd5" '(harpoon-delete-5    :which-key "Delete Harpoon Mark 5")
  "hd6" '(harpoon-delete-6    :which-key "Delete Harpoon Mark 6")
  "hd7" '(harpoon-delete-7    :which-key "Delete Harpoon Mark 7")
  "hd8" '(harpoon-delete-8    :which-key "Delete Harpoon Mark 8")
  "hd9" '(harpoon-delete-9    :which-key "Delete Harpoon Mark 9")
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
  (forge-add-default-bindings nil)
  )

;; Forge
(use-package forge
  :after
  (magit)
  )

;; Org-mode
(use-package org
  :commands
  (org-capture org-agenda)
  :preface
  (defun arbab/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
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
  :custom
  (org-agenda-span 10)
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-with-log-mode t)
  (org-confirm-babel-evaluate nil)
  (org-ellipsis "▾")
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-startup-with-inline-images nil)
  (org-image-actual-width 600)
  (org-hide-emphasis-markers t)
  (org-link-descriptive t)
  (org-pretty-entities nil)
  (org-hidden-keywords nil)
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
  :init
  (org-super-agenda-mode +1)
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
  )

;; Org-wild-notifier
(use-package org-wild-notifier
  :after
  (org)
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
  (org)
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

;; Org-sidebar
(use-package org-sidebar
  :after
  (org)
  :commands
  (
   org-sidebar-tree-toggle
   org-sidebar-toggle
   )
  )

;; Org-appear
(use-package org-appear
  :after
  (org)
  :hook
  (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers nil)
  (org-appear-autoentities t)
  (org-appear-autokeywords nil)
  (org-appear-delay 0)
  )

;; Org-roam
(use-package org-roam
  :after
  (org)
  :config
  (org-roam-setup)
  :custom
  (org-roam-v2-ack t)
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
  (org)
  :hook
  (org-mode . toc-org-mode)
  )

;; Org-babel-templates
(require 'org-tempo)
(setq org-structure-template-alist
      '(
        ("lua" . "src lua")
        ("py" . "src python")
        ("el" . "src emacs-lisp")
        ("sh" . "src shell")
        ("a" . "export ascii")
        ("c" . "center")
        ("C" . "comment")
        ("e" . "example")
        ("E" . "export")
        ("h" . "export html")
        ("l" . "export latex")
        ("q" . "quote")
        ("s" . "src")
        ("v" . "verse")
        )
      )

;; Ob-async
(use-package ob-async)

;; Visual-fill-column
(use-package visual-fill-column
  :commands
  (visual-fill-column-mode global-visual-fill-column-mode)
  :preface
  (defun arbab/org-mode-visual-fill ()
    (setq visual-fill-column-width 150)
    (visual-fill-column-mode +1)
    )
  :config
  (setq-default visual-fill-column-center-text t)
  ;; :hook
  ;; (org-mode . arbab/org-mode-visual-fill)
  )

;; Org-remoteimg
(use-package org-remoteimg
  :straight
  (
   :type git
   :host github
   :repo "gaoDean/org-remoteimg"
   )
  :custom
  (org-display-remote-inline-images 'cache)
  (url-cache-directory "~/.cache/emacs/url")
  (url-automatic-caching t)
  )

;; Org-imgtog
(use-package org-imgtog
  :disabled t
  :straight
  (
   :type git
   :host github
   :repo "gaoDean/org-imgtog"
   )
  :hook
  (org-mode . org-imgtog-mode)
  )

;; Haskell-mode
(use-package haskell-mode
  :mode
  ("\\.hs\\’")
  )

;; Lua-mode
(use-package lua-mode
  :mode
  ("\\.lua\\’")
  )

;; Rust-mode
(use-package rust-mode
  :mode
  ("\\.rs\\’")
  )

;; Json-mode
(use-package json-mode
  :mode
  ("\\.json\\’")
  )

;; Typescript-mode
(use-package typescript-mode
  :mode
  ("\\.ts\\'")
  :custom
  (typescript-indent-level 2)
  )

;; Tldr
(use-package tldr
  :commands
  (tldr)
  )

;; Vterm
(use-package vterm
  :bind
  ("M-RET" . vterm)
  :custom
  (vterm-max-scrollback 10000)
  )

;; Markdown-mode
(use-package markdown-mode
  :mode
  ("\\.md\\'")
  )

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
  :commands
  (lsp lsp-deferred)
  :hook
  (prog-mode . arbab/lsp-mode-setup)
  (lsp-mode . (lambda ()
                (let ((lsp-keymap-prefix "C-c l"))
                  (lsp-enable-which-key-integration))))
  :custom
  (lsp-enable-which-key-integration t)
  (lsp-lens-enable nil)
  (lsp-enable-symbol-highlighting nil)
  :config
  (define-key lsp-mode-map (kbd lsp-keymap-prefix) nil)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  )

;; Lsp-ivy
(use-package lsp-ivy
  :after
  (lsp-mode)
  )

;; LSP-UI
(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  ;; Ui-doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-position 'top-right-corner)
  (lsp-ui-doc-delay 5)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-enhanced-markdown t)
  (lsp-ui-doc-use-childframe t)
  ;; Sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-delay 0.0)
  )

;; Sideline
(use-package sideline
  :hook
  (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'point)
  (sideline-backends-right '(
                             sideline-flymake
                             ))
  )
;; Sideline-flymake
(use-package sideline-flymake
  :after
  (sideline)
  )

;; LSP-Haskell
(use-package lsp-haskell
  :after
  (lsp-mode)
  )

;; LSP-pyright
(use-package lsp-pyright
  :after
  (lsp-mode)
  )

;; LSP-treemacs
(use-package lsp-treemacs
  :after
  (lsp-mode)
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
  (centaur-tabs-mode t)
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
  :custom
  (centaur-tabs-set-bar 'left)
  (centaur-tabs-style "zigzag")
  (centaur-tabs-cycle-scope 'default)
  (centaur-tabs-height 40)
  (centaur-tabs-set-icons t)
  (centaur-tabs-close-button "")
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "")
  (centaur-tabs-show-new-tab-button t)
  (centaur-tabs-new-tab-text "  ")
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
  :mode
  ("\\.yuck\\'")
  )

;; Parinfer-rust-mode
(use-package parinfer-rust-mode
  :defer t
  :custom
  (parinfer-rust-auto-download t)
  )

;; Emmet
(use-package emmet-mode
  :hook
  (html-mode . emmet-mode)
  )

;; Undo-fu
(use-package undo-fu
  :config
  (setq undo-outer-limit 1006632960) ; 960mb
  )

;; Undo-fu-session
(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-linear nil)
  )

;; Vundo
(use-package vundo
  :custom
  (vundo-compact-display t)
  (vundo-glyph-alist vundo-unicode-symbols)
  :config
  ;; Better contrasting highlight.
  (custom-set-faces
   '(vundo-node ((t (:foreground "#808080"))))
   '(vundo-stem ((t (:foreground "#808080"))))
   '(vundo-highlight ((t (:foreground "#FFFF00"))))
   )
  ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
  :bind
  (
   ("C-x u" . vundo)
   :map vundo-mode-map
   ("l"       . vundo-forward)
   ("h"       . vundo-backward)
   ("j"       . vundo-next)
   ("k"       . vundo-previous)
   ("<right>" . vundo-forward)
   ("<left>"  . vundo-backward)
   ("<down>"  . vundo-next)
   ("<up>"    . vundo-previous)
   ("<home>"  . vundo-stem-root)
   ("<end>"   . vundo-stem-end)
   ("q"       . vundo-quit)
   ("C-g"     . vundo-quit)
   ("RET"     . vundo-confirm)
   )
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
  ("C-c r a" . quickrun-with-arg)
  ("C-c r s" . quickrun-select)
  )

;; Corral
(use-package corral
  :custom
  (corral-preserve-point t)
  )

;; Nov
(use-package nov
  :preface
  (defun arbab/nov-mode-setup ()
    (face-remap-add-relative 'variable-pitch :family "Cantarell" :height 1.0)
    (visual-fill-column-mode)
    (visual-line-mode)
    )
  :custom
  (nov-variable-pitch t)
  :mode
  ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode . arbab/nov-mode-setup)
  )

;; Vimish-fold
(use-package vimish-fold
  :after
  (evil)
  )

;; Evil-vimish-fold
(use-package evil-vimish-fold
  :after
  (vimish-fold)
  :config
  (global-evil-vimish-fold-mode)
  :custom
  (evil-vimish-fold-mode-lighter "")
  (evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  )

;; Evil-numbers
(use-package evil-numbers
  :bind
  (
   :map evil-normal-state-map
   ("C-c a" . evil-numbers/inc-at-pt)
   ("C-c d" . evil-numbers/dec-at-pt)
   )
  )

;; Burly
(use-package burly
  :defer t
  )

;; Highlight-indentation
(use-package highlight-indent-guides
  :disabled t
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  )

;; Sxhkd-mode
(use-package sxhkdrc-mode
  :custom
  (sxhkd-mode-reload-config t)
  :mode
  ("\\sxhkdrc\\’" . sxhkdrc-mode)
  )

;; Minimap
(use-package minimap
  :commands
  (minimap-mode)
  :custom
  (minimap-window-location 'right)
  (minimap-minimum-width 10)
  (minimap-dedicated-window t)
  (minimap-hide-cursor t)
  (minimap-hide-scroll-bar nil)
  (minimap-hide-fringes t)
  )

;; Smartparens
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  )

;; Prescient
(use-package prescient
  :custom
  (prescient-persist-mode t)
  (prescient-sort-length-enable nil)
  )

;; Ivy-prescient
(use-package ivy-prescient
  :after
  (counsel)
  :config
  (ivy-prescient-mode +1)
  :custom
  (ivy-prescient-retain-classic-highlighting nil)
  )

;; Company-prescient
(use-package company-prescient
  :after
  (company)
  :config
  (company-prescient-mode +1)
  )

;; Smex
(use-package amx
  :after
  (counsel)
  )

;; Treesit-auto
(use-package treesit-auto
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode)
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
  :mode
  ("\\.vim\\'")
  )

;; Nix-mode
(use-package nix-mode
  :mode
  ("\\.nix\\'")
  )

;; Diminish
(use-package diminish)

;; Highlight-numbers
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
  :custom
  (popup-kill-ring-interactive-insert t)
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
  :config
  (smart-hungry-delete-add-default-hooks)
  :bind
  (
   ([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
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
(use-package link-hint
  :defer t
  )

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
  :init
  (ace-window-display-mode +1)
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
  :commands
  (centered-cursor-mode global-centered-cursor-mode)
  )

;; Git-gutter
(use-package git-gutter
  :hook
  (
   (
    prog-mode
    text-mode
    ) . git-gutter-mode)
  :custom
  (git-gutter:update-interval 2)
  (git-gutter:window-width 2)
  (git-gutter:modified-sign "!")
  (git-gutter:added-sign "")
  (git-gutter:deleted-sign "")
  )

;; Goto-line-preview
(use-package goto-line-preview
  :bind
  (
   ([remap goto-line] . goto-line-preview)
   )
  )

;; Fancy-compilation
(use-package fancy-compilation
  :hook
  (compilation-mode . fancy-compilation-mode)
  )

;; Imenu-list
(use-package imenu-list
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t)
  :bind
  (
   ("<f10>" . imenu-list-smart-toggle)
   )
  )

;; Deadgrep
;; Install ripgrep on your local system before using.
(use-package deadgrep
  :bind
  (
   ("<f5>" . deadgrep)
   :map deadgrep-mode-map
   ("M-c" . deadgrep-cycle-search-case)
   ("M-t" . deadgrep-cycle-search-type)
   ("M-s" . deadgrep-incremental)
   ("M-j" . deadgrep-forward-match)
   ("M-k" . deadgrep-backward-match)
   ("M-f" . deadgrep-directory)
   ("M-F" . deadgrep-toggle-file-results)
   )
  )

;; Rg
(use-package rg
  :bind
  (
   ("C-c s" . rg-menu)
   )
  )

;; Ligature
(use-package ligature
  :hook
  (
   (
    prog-mode
    text-mode
    ) . ligature-mode)
  :config
  ;; This variable is based on the font your are using. For me it is ’JetBrains Mono Nerd Font'. If you use any other font, make sure to check this https://github.com/mickeynp/ligature.el/wiki
  (ligature-set-ligatures 'prog-mode
                          '
                          (
                           "-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/==" "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||""<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*""<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<""..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##" "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_""__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"
                           )
                          )
  )

;; Hl-todo
(use-package hl-todo
  :defer t
  :custom
  (setq hl-todo-keyword-faces
        '(
          ("TODO"   . "#b48ead")
          ("FIXME"  . "#bf616a")
          ("DEBUG"  . "#d08770")
          ("NOTE"   . "#a3be8c")
          )
        )
  )

;; Magit-todos
(use-package magit-todos
  :hook
  (magit-status-mode . magit-todos-mode)
  )

;; Blamer
(use-package blamer
  :commands
  (blamer-mode)
  :hook
  (
   (
    prog-mode
    text-mode
    ) . blamer-mode)
  :bind
  (
   ("C-c i" . blamer-show-posframe-commit-info)
   )
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 0)
  (blamer-max-commit-message-length 30)
  (blamer-view 'overlay)
  (blamer-prettify-time-p t)
  (blamer-type 'both)
  (blamer-show-avatar-p nil)
  :custom-face
  (blamer-face ((t
                 :foreground "#b48ead"
                 :background nil
                 :height 125
                 :italic t
                 )
                )
               )
  )

;; Ts-fold
(use-package ts-fold
  :preface
  (defun arbab/ts-fold-mode ()
    "Activate a minor mode only in buffers using tree-sitter-based major modes."
    (when (derived-mode-p
           'bash-ts-mode
           'c++-ts-mode
           'c-or-c++-mode-ts-mode
           'c-ts-mode
           'cmake-ts-mode
           'csharp-ts-mode
           'css-ts-mode
           'dockerfile-ts-mode
           'go-mod-ts-mode
           'go-ts-mode
           'java-ts-mode
           'js-ts-mode
           'json-ts-mode
           'python-ts-mode
           'ruby-ts-mode
           'rust-ts-mode
           'toml-ts-mode
           'tsx-ts-mode
           'typescript-ts-mode
           'yaml-ts-mode
           )
      (ts-fold-mode)
      )
    )

  :straight
  (
   :type git
   :host github
   :repo "AndrewSwerlick/ts-fold"
   :branch "andrew-sw/treesit-el-support"
   )
  :hook
  (prog-mode . arbab/ts-fold-mode)
  (ts-fold-mode . (lambda () (evil-define-key 'normal 'local (kbd "<tab>") 'ts-fold-toggle)))
  )

;; Indent-bars
(use-package indent-bars
  :straight
  (
   :type git
   :host github
   :repo "jdtsmith/indent-bars"
   )
  :hook
  (prog-mode . indent-bars-mode)
  :config
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.6)
   indent-bars-pattern " . . . . ." ; play with the number of dots for your usual font size
   indent-bars-width-frac 0.25
   indent-bars-pad-frac 0.1)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  )

;; Harpoon
(use-package harpoon)

;; Evil-multiedit
(use-package evil-multiedit
  :config
  (evil-define-key 'normal 'global
    (kbd "C-q q")  #'evil-multiedit-match-symbol-and-next
    (kbd "C-q Q")  #'evil-multiedit-match-symbol-and-prev
    )
  (evil-define-key 'visual 'global
    (kbd "C-q r")  #'evil-multiedit-match-all
    (kbd "C-q q")  #'evil-multiedit-match-and-next
    (kbd "C-q Q" ) #'evil-multiedit-match-and-prev
    )
  (evil-define-key '(visual normal) 'global
    (kbd "C-M-d") #'evil-multiedit-restore
    )
  (with-eval-after-load 'evil-mutliedit
    (evil-define-key 'multiedit 'global
      (kbd "C-q q") #'evil-multiedit-match-and-next
      (kbd "C-q Q") #'evil-multiedit-match-and-prev
      (kbd "RET")   #'evil-multiedit-toggle-or-restrict-region
      )
    (evil-define-key '(multiedit multiedit-insert) 'global
      (kbd "M-j")  #'evil-multiedit-next
      (kbd "M-k")  #'evil-multiedit-prev
      )
    )
  )

;; Auto-rename-tag
(use-package auto-rename-tag
  :hook
  (
   (
    html-mode
    xml-mode
    ) . auto-rename-tag-mode
   )
  )

;; Golden-ratio
(use-package golden-ratio
  :commands
  (golden-ratio golden-ratio-mode)
  )

;; Yasnippet
(use-package yasnippet
  :hook
  (
   (
    text-mode
    prog-mode
    ) . yas-minor-mode
   )
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  )

;; yasnippet-snippets
(use-package yasnippet-snippets
  :after
  (yasnippet)
  )

;; Ivy-yasnippet
(use-package ivy-yasnippet
  :after
  (yasnippet)
  :bind
  (
   ("C-c y" . ivy-yasnippet)
   )
  )
