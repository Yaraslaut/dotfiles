;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font-increment 1)
(setq me/doom-font-size 14)
(setq doom-font (font-spec :family "MonaspiceKrNerdFont" :size me/doom-font-size))
(setq doom-theme 'doom-acario-dark)

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


(setq system-clangd "/bin/clangd")

(use-package! lsp  ; `use-package!' is a thin wrapper around `use-package'
                       ; it is required that you use this in Doom's modules,
                       ; but not required to be used in your private config.
  :config
  (setq

   lsp-clients-clangd-executable system-clangd
   lsp-clients-clangd-args '("--header-insertion-decorators=0" "--pch-storage=disk" "--clang-tidy")
))

(add-hook 'after-save-hook 'lsp-format-buffer)
