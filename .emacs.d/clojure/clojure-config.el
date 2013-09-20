(require 'clojure-mode)
(require 'nrepl)
 
;; Configure nrepl.el
(setq nrepl-hide-special-buffers t)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")
 
;; Some default eldoc facilities
(add-hook 'nrepl-connected-hook
(defun pnh-clojure-mode-eldoc-hook ()
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(nrepl-enable-on-existing-clojure-buffers)))
 
;; Repl mode hook
(add-hook 'nrepl-mode-hook 'subword-mode)

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("gantryfile" . clojure-mode))
(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl nil)

(require 'nrepl-ritz) ;; after (require 'nrepl)
(add-hook 'nrepl-interaction-mode-hook 'my-nrepl-mode-setup)
(defun my-nrepl-mode-setup ()
  (require 'nrepl-ritz))

;; Ritz middleware
(define-key nrepl-interaction-mode-map (kbd "C-c C-j") 'nrepl-javadoc)
(define-key nrepl-mode-map (kbd "C-c C-j") 'nrepl-javadoc)
(define-key nrepl-interaction-mode-map (kbd "C-c C-a") 'nrepl-apropos)
(define-key nrepl-mode-map (kbd "C-c C-a") 'nrepl-apropos)

(defun sldb ()
    (interactive)
    (insert "(throw (Throwable. \"Call debugger\"))")
    (nrepl-ritz-break-on-exception "true"))

(global-set-key (kbd "C-c s l d") 'sldb)

(defun alembic ()
    (interactive)
    (insert "(require '[alembic.still :as alembic]) (alembic/load-project)"))

(global-set-key (kbd "C-c c a l") 'alembic)

(defun refresh ()
    (interactive)
    (insert "(require '[clojure.tools.namespace.repl :refer [refresh]]) (refresh)"))

(defun pprint ()
  (interactive)
  (insert "(require '[clojure.pprint :refer [pprint]])"))

(defun refresh-nrepl ()
  (interactive)
  (pprint)
  (refresh))

(global-set-key (kbd "C-c c r f") 'refresh-nrepl)
(global-set-key (kbd "C-c c l p p") 'pprint)

;; (defun turn-on-paredit () (paredit-mode 1))
;; (add-hook 'clojure-mode-hook 'turn-on-paredit)

;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))
