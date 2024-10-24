;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font-increment 1)
(setq me/doom-font-size 14)


;;; check system name


;;; only for not Green.local
(cond ( (equal system-name "bahamankolibri.lin.tuni.fi")
        (setq copilot-node-executable "/worktmp/yaraslau/programs/node-v20.12.2-linux-x64/bin/node") )
)      


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



(defun insert-quotes (open close)
  (if (region-active-p)
      (progn
        (let* ((mark-start (region-beginning))
               (mark-end (region-end)))
          (goto-char mark-end)
          (insert close)
          (goto-char mark-start)
          (insert open)))
    (insert open close))
  (backward-char))

;(global-set-key (kbd "C-c e") (lambda() (interactive) (insert-quotes  "\"" "\\n\"")))
