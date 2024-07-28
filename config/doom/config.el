;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font-increment 1)
(setq me/doom-font-size 14)


;;; check system name


;;; only for not Green.local
(cond ( (equal system-name "Green.local") )
      ( t
        (setq doom-font (font-spec :family "MonaspiceKr Nerd Font" :size me/doom-font-size))
        )
)

(setq doom-theme 'doom-acario-dark)

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


(cond
     ( (equal system-name "Green.local") (setq system-clangd "/opt/homebrew/opt/llvm/bin/clangd") )
     ( t  (setq system-clangd "/usr/bin/clangd") )
)

(use-package! lsp  ; `use-package!' is a thin wrapper around `use-package'
                       ; it is required that you use this in Doom's modules,
                       ; but not required to be used in your private config.
  :config
  (setq
   lsp-clients-clangd-executable system-clangd
   lsp-clients-clangd-args '("--header-insertion-decorators=0" "--pch-storage=disk" "--clang-tidy")
))



;;; only for not Green.local
(cond ( (equal system-name "Green.local") )
      ( t (add-hook 'after-save-hook 'lsp-format-buffer))
)
