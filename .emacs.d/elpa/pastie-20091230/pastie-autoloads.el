;;; pastie-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pastie-browse pastie-get pastie-buffer pastie-region)
;;;;;;  "pastie" "pastie.el" (19840 13784))
;;; Generated autoloads from pastie.el

(autoload 'pastie-region "pastie" "\
Post the current region as a new paste at pastie.org.
Copies the URL into the kill ring.

With a prefix argument, toggle the current value of
`*pastie-restricted*'.

\(fn BEGIN END &optional TOGGLE-RESTRICTED)" t nil)

(autoload 'pastie-buffer "pastie" "\
Post the current buffer as a new paste at pastie.org.
Copies the URL into the kill ring.

With a prefix argument, toggle the current value of
`*pastie-restricted*'.

\(fn &optional TOGGLE-RESTRICTED)" t nil)

(autoload 'pastie-get "pastie" "\
Fetch the contents of the paste from pastie.org into a new buffer.

\(fn ID)" t nil)

(autoload 'pastie-browse "pastie" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("pastie-pkg.el") (19840 13784 463508))

;;;***

(provide 'pastie-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pastie-autoloads.el ends here
