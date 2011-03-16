;;; blank-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (blank-global-mode-off blank-global-mode-on blank-global-mode
;;;;;;  blank-mode-off blank-mode-on blank-mode blank-mode-customize)
;;;;;;  "blank-mode" "blank-mode.el" (19840 13715))
;;; Generated autoloads from blank-mode.el

(autoload 'blank-mode-customize "blank-mode" "\
Customize blank-mode options.

\(fn)" t nil)

(autoload 'blank-mode "blank-mode" "\
Toggle blank minor mode visualisation (bl on modeline).

If ARG is null, toggle blank visualisation.
If ARG is a number and is greater than zero, turn on visualisation; otherwise,
turn off visualisation.

\(fn &optional ARG)" t nil)

(autoload 'blank-mode-on "blank-mode" "\
Turn on blank minor mode visualisation (bl on modeline).

\(fn)" t nil)

(autoload 'blank-mode-off "blank-mode" "\
Turn off blank minor mode visualisation (bl on modeline).

\(fn)" t nil)

(autoload 'blank-global-mode "blank-mode" "\
Toggle blank global minor mode visualisation (BL on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system.

\(fn &optional ARG)" t nil)

(autoload 'blank-global-mode-on "blank-mode" "\
Turn on blank global minor mode visualisation (BL on modeline).

\(fn)" t nil)

(autoload 'blank-global-mode-off "blank-mode" "\
Turn off blank global minor mode visualisation (BL on modeline).

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("blank-mode-pkg.el") (19840 13715 578839))

;;;***

(provide 'blank-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; blank-mode-autoloads.el ends here
