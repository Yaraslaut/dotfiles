;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-dark+)
;;(setq doom-theme 'doom-acario-light)
(setq doom-font-increment 1)
(setq me/doom-font-size 14)
(setq display-line-numbers-mode t)

;; set clang executable for different systems
;; Green.local - > "/opt/homebrew/opt/llvm/bin/clangd"
;; bahamankolibri.lin.tuni.fi -> "/usr/bin/clangd"
;; Cartan -> /home/yaraslau/.local/llvm-19/bin/clangd
(setq system-clang "/usr/bin/clang")
(cond
     ( (equal (system-name) "Green.local")                (setq system-clangd "/opt/homebrew/opt/llvm/bin/clangd") )
     ( (equal (system-name) "bahamankolibri.lin.tuni.fi") (setq system-clangd "/usr/bin/clangd") )
     ( (equal (system-name) "Cartan" )                    (setq system-clangd "/home/yaraslau/.local/llvm-19/bin/clangd") )
)

;;; only for not Green.local
(cond ( (equal (system-name) "Green.local") () )
      ( t (add-hook 'after-save-hook 'lsp-format-buffer))
)


(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package! lsp
  :config
  (setq
   lsp-clients-clangd-executable system-clangd
   lsp-clients-clangd-args '("--header-insertion-decorators=0" "--pch-storage=disk" "--clang-tidy")))

(cond ( (equal (system-name) "bahamankolibri.lin.tuni.fi")
        (setq copilot-node-executable "/worktmp/yaraslau/programs/node-v20.12.2-linux-x64/bin/node") )
)      

(cond ( (equal (system-name) "Green.local") )
      ( t
        (setq doom-font (font-spec :family "MonaspiceKr Nerd Font" :size me/doom-font-size))
        )
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
