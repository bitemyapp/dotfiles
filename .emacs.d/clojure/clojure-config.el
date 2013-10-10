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
    (insert "(require '[clojure.tools.namespace.repl :refer [refresh]]) (refresh) "))

(defun pprint ()
  (interactive)
  (insert "(require '[clojure.pprint :refer [pprint]]) "))

(defun diff ()
  (interactive)
  (insert "(require '[clojure.data :refer [diff]]) "))

(defun repl ()
  (interactive)
  (insert "(require '[clojure.repl :refer :all])"))

(defun refresh-nrepl ()
  (interactive)
  (refresh)
  (pprint)
  (diff)
  (repl))

(defun st-on ()
  (interactive)
  (setq nrepl-popup-stacktraces-in-repl t))

(defun st-off ()
  (interactive)
  (setq nrepl-popup-stacktraces-in-repl nil))

(defun inject-trace ()
  (interactive)
  (insert "(use 'clojure.tools.trace)"))

(defun trace-on ()
  (interactive)
  (replace-string "defn" "deftrace" nil (region-beginning) (region-end)))

(defun trace-off ()
  (interactive)
  (replace-string "deftrace" "defn" nil (region-beginning) (region-end)))

;; (alter-var-root #'*compiler-options* assoc :disable-locals-clearing true)
;; (require '[alex-and-georges.debug-repl :refer [debug-repl quit-dr]])

(global-set-key (kbd "C-c c r f") 'refresh-nrepl)
(global-set-key (kbd "C-c c l p p") 'pprint)
(global-set-key (kbd "C-c c s t o") 'st-on)
(global-set-key (kbd "C-c c s t f") 'st-off)
(global-set-key (kbd "C-c c i t f") 'inject-trace)
(global-set-key (kbd "C-c c t r o") 'trace-on)
(global-set-key (kbd "C-c c t r f") 'trace-off)

(defun local-nrepl ()
  (interactive)
  (let ((port (read-from-minibuffer "Port? " nil)))
    (nrepl "localhost" port)))

(global-set-key (kbd "C-c c l n") 'local-nrepl)
(global-set-key (kbd "C-c c n q") 'nrepl-quit)

;; (defun turn-on-paredit () (paredit-mode 1))
;; (add-hook 'clojure-mode-hook 'turn-on-paredit)

;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))
