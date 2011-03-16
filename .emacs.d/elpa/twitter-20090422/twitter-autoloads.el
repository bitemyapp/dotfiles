;;; twitter-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (twitter-status-edit twitter-get-friends-timeline)
;;;;;;  "twitter" "twitter.el" (19840 13818))
;;; Generated autoloads from twitter.el

(autoload 'twitter-get-friends-timeline "twitter" "\
Fetch and display the friends timeline.
The results are formatted and displayed in a buffer called
*Twitter friends timeline*

If the variable `twitter-include-replies' is non-nil, the replies
timeline will also be merged into the friends timeline and
displayed.

\(fn)" t nil)

(autoload 'twitter-status-edit "twitter" "\
Edit your twitter status in a new buffer.
A new buffer is popped up in a special edit mode. Press
\\[twitter-status-post] when you are finished editing to send the
message.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("twitter-pkg.el") (19840 13818 744194))

;;;***

(provide 'twitter-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; twitter-autoloads.el ends here
