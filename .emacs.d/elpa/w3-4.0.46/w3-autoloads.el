;;; w3-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (x-font-build-cache font-default-size-for-device
;;;;;;  font-default-encoding-for-device font-default-registry-for-device
;;;;;;  font-default-family-for-device font-default-object-for-device
;;;;;;  font-default-font-for-device font-create-object) "font" "font.el"
;;;;;;  (19848 47716))
;;; Generated autoloads from font.el

(autoload 'font-create-object "font" "\
Not documented

\(fn FONTNAME &optional DEVICE)" nil nil)

(autoload 'font-default-font-for-device "font" "\
Not documented

\(fn &optional DEVICE)" nil nil)

(autoload 'font-default-object-for-device "font" "\
Not documented

\(fn &optional DEVICE)" nil nil)

(autoload 'font-default-family-for-device "font" "\
Not documented

\(fn &optional DEVICE)" nil nil)

(autoload 'font-default-registry-for-device "font" "\
Not documented

\(fn &optional DEVICE)" nil nil)

(autoload 'font-default-encoding-for-device "font" "\
Not documented

\(fn &optional DEVICE)" nil nil)

(autoload 'font-default-size-for-device "font" "\
Not documented

\(fn &optional DEVICE)" nil nil)

(autoload 'x-font-build-cache "font" "\
Not documented

\(fn &optional DEVICE)" nil nil)

;;;***

;;;### (autoloads (w3-prev-document w3-next-document w3-follow-link
;;;;;;  w3-follow-link-other-frame w3-do-setup w3 w3-version w3-preview-this-buffer
;;;;;;  w3-follow-url-at-point w3-follow-url-at-point-other-frame
;;;;;;  w3-maybe-follow-link w3-maybe-follow-link-mouse w3-fetch
;;;;;;  w3-fetch-other-frame w3-find-file w3-open-local) "w3" "w3.el"
;;;;;;  (19848 47717))
;;; Generated autoloads from w3.el

(autoload 'w3-open-local "w3" "\
Find a local file, and interpret it as a hypertext document.
Prompt for an existing file or directory, and retrieve it as a
hypertext document.

\(fn FNAME)" t nil)

(autoload 'w3-find-file "w3" "\
Find a local file, and interpret it as a hypertext document.
Prompt for an existing file or directory, and retrieve it as a
hypertext document.

\(fn FNAME)" t nil)

(autoload 'w3-fetch-other-frame "w3" "\
Attempt to follow the hypertext reference under point in a new frame.

\(fn &optional URL)" t nil)

(autoload 'w3-fetch "w3" "\
Retrieve a document over the World Wide Web.
Defaults to URL of the current document, if any.
With prefix argument, use the URL of the hyperlink under point instead.

\(fn &optional URL TARGET)" t nil)

(autoload 'w3-maybe-follow-link-mouse "w3" "\
Maybe follow a hypertext link under point.
If there is no link under point, this will try using
`url-get-url-at-point'

\(fn E)" t nil)

(autoload 'w3-maybe-follow-link "w3" "\
Maybe follow a hypertext link under point.
If there is no link under point, this will try using
`url-get-url-at-point'

\(fn)" t nil)

(autoload 'w3-follow-url-at-point-other-frame "w3" "\
Follow the URL under PT, defaults to link under (point).

\(fn &optional PT)" t nil)

(autoload 'w3-follow-url-at-point "w3" "\
Follow the URL under PT, defaults to link under (point).

\(fn &optional PT)" t nil)

(autoload 'w3-preview-this-buffer "w3" "\
See what this buffer will look like when its formatted as HTML.
HTML is the HyperText Markup Language used by the World Wide Web to
specify formatting for text.  More information on HTML can be found at
ftp.w3.org:/pub/www/doc.

\(fn)" t nil)

(autoload 'w3-version "w3" "\
Show the version number of W3 in the minibuffer.
If optional argument HERE is non-nil, insert info at point.

\(fn &optional HERE)" t nil)

(autoload 'w3 "w3" "\
Retrieve the default World Wide Web home page.
The World Wide Web is a global hypertext system started by CERN in
Switzerland in 1991.

The home page is specified by the variable `w3-default-homepage'.  The
document should be specified by its fully specified Uniform Resource
Locator.  The document will be parsed as HTML (if appropriate) and
displayed in a new buffer.

\(fn)" t nil)

(autoload 'w3-do-setup "w3" "\
Do setup.
This is to avoid conflict with user settings when W3 is dumped with
Emacs.

\(fn)" nil nil)

(autoload 'w3-follow-link-other-frame "w3" "\
Attempt to follow the hypertext reference under point in a new frame.
With prefix-arg P, ignore viewers and dump the link straight
to disk.

\(fn &optional P)" nil nil)

(autoload 'w3-follow-link "w3" "\
Attempt to follow the hypertext reference under point.
With prefix-arg P, ignore viewers and dump the link straight
to disk.

\(fn &optional P)" t nil)

(autoload 'w3-next-document "w3" "\
Not documented

\(fn)" t nil)

(autoload 'w3-prev-document "w3" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (turn-on-w3-dired w3-dired-find-file-dired w3-dired-minor-mode)
;;;;;;  "w3-dired" "w3-dired.el" (19848 47716))
;;; Generated autoloads from w3-dired.el

(autoload 'w3-dired-minor-mode "w3-dired" "\
Minor mode for directory browsing with Emacs-W3.

\(fn &optional ARG)" t nil)

(autoload 'w3-dired-find-file-dired "w3-dired" "\
\"Edit\" directory DIR, but with additional URL-friendly bindings.

\(fn DIR)" t nil)

(autoload 'turn-on-w3-dired "w3-dired" "\
Unconditionally turn on W3 Dired bindings.

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3-region) "w3-display" "w3-display.el" (19848
;;;;;;  47716))
;;; Generated autoloads from w3-display.el

(autoload 'w3-region "w3-display" "\
Parse and display the region of this buffer between ST and ND.

\(fn ST ND)" t nil)

;;;***

;;;### (autoloads (w3-do-text-entry w3-form-resurrect-widgets w3-form-add-element)
;;;;;;  "w3-forms" "w3-forms.el" (19848 47716))
;;; Generated autoloads from w3-forms.el

(autoload 'w3-form-add-element "w3-forms" "\
Not documented

\(fn PLIST FACE)" nil nil)

(autoload 'w3-form-resurrect-widgets "w3-forms" "\
Not documented

\(fn)" nil nil)

(autoload 'w3-do-text-entry "w3-forms" "\
Not documented

\(fn WIDGET &rest IGNORE)" nil nil)

;;;***

;;;### (autoloads (w3-hotlist-view w3-hotlist-apropos w3-hotlist-refresh
;;;;;;  w3-hotlist-delete w3-hotlist-add-document w3-hotlist-add-document-at-point
;;;;;;  w3-use-hotlist w3-parse-hotlist w3-read-html-bookmarks) "w3-hot"
;;;;;;  "w3-hot.el" (19848 47716))
;;; Generated autoloads from w3-hot.el

(autoload 'w3-read-html-bookmarks "w3-hot" "\
Import an HTML file into the Emacs-w3 format.

\(fn FNAME)" t nil)

(autoload 'w3-parse-hotlist "w3-hot" "\
Read in the hotlist specified by FNAME

\(fn &optional FNAME)" nil nil)

(autoload 'w3-use-hotlist "w3-hot" "\
Possibly go to a link in your W3/Mosaic hotlist.
This is part of the emacs World Wide Web browser.  It will prompt for
one of the items in your 'hotlist'.  A hotlist is a list of often
visited or interesting items you have found on the World Wide Web.

\(fn)" t nil)

(autoload 'w3-hotlist-add-document-at-point "w3-hot" "\
Add the document pointed to by the hyperlink under point to the hotlist.

\(fn PREF-ARG)" t nil)

(autoload 'w3-hotlist-add-document "w3-hot" "\
Add this documents url to the hotlist

\(fn PREF-ARG &optional THE-TITLE THE-URL)" t nil)

(autoload 'w3-hotlist-delete "w3-hot" "\
Deletes a document from your hotlist file

\(fn)" t nil)

(autoload 'w3-hotlist-refresh "w3-hot" "\
Reload the default hotlist file into memory

\(fn)" t nil)

(autoload 'w3-hotlist-apropos "w3-hot" "\
Show hotlist entries matching REGEXP.

\(fn REGEXP)" t nil)

(autoload 'w3-hotlist-view "w3-hot" "\
Show the hotlist.

\(fn)" t nil)

;;;***

;;;### (autoloads (w3-hotindex-query w3-hotindex-delete-entry w3-hotindex-rename-entry
;;;;;;  w3-hotindex-rm-key w3-hotindex-add-key) "w3-hotindex" "w3-hotindex.el"
;;;;;;  (19848 47716))
;;; Generated autoloads from w3-hotindex.el

(autoload 'w3-hotindex-add-key "w3-hotindex" "\
*Add a keyword to an item in w3-hotindex. Completion is done
on the list of all keywords.

\(fn NAME KEYWORD)" t nil)

(autoload 'w3-hotindex-rm-key "w3-hotindex" "\
*Remove a keyword from an item of w3-hotindex.

\(fn ENTRY KEYWORD)" t nil)

(autoload 'w3-hotindex-rename-entry "w3-hotindex" "\
Renames an entry in the HotIndex. Intended to be called from 
w3-hotlist-rename-entry. OLD should equal the entry to be renamed.
Case is therefore important.

\(fn OLD NEW)" nil nil)

(autoload 'w3-hotindex-delete-entry "w3-hotindex" "\
Deletes an entry in the HotIndex. Intended to be called from 
w3-hotlist-delete. OLD should equal the entry to be deleted.
Case is therefore important.

\(fn TITLE)" nil nil)

(autoload 'w3-hotindex-query "w3-hotindex" "\
Query the HotIndex for KEY.

\(fn KEY)" t nil)

;;;***

;;;### (autoloads (w3-show-dvi w3-parse-tree-to-latex) "w3-latex"
;;;;;;  "w3-latex.el" (19848 47717))
;;; Generated autoloads from w3-latex.el

(autoload 'w3-parse-tree-to-latex "w3-latex" "\
Not documented

\(fn TREE &optional URL)" nil nil)

(autoload 'w3-show-dvi "w3-latex" "\
Uses xdvi to show DVI file created from `w3-parse-tree-to-latex'.

\(fn)" t nil)

;;;***

;;;### (autoloads (w3-print-this-url) "w3-print" "w3-print.el" (19848
;;;;;;  47717))
;;; Generated autoloads from w3-print.el

(autoload 'w3-print-this-url "w3-print" "\
Print out the current document

\(fn &optional URL FORMAT)" t nil)

;;;***

;;;### (autoloads (w3-table-setup-keys w3-table-speak-current-table-column)
;;;;;;  "w3-speak-table" "w3-speak-table.el" (19848 47717))
;;; Generated autoloads from w3-speak-table.el

(autoload 'w3-table-speak-current-table-column "w3-speak-table" "\
Speak current table column. Prefix arg can be used to specify the desired table nesting.

\(fn &optional AT-DEPTH TABLE-INFO)" t nil)

(autoload 'w3-table-setup-keys "w3-speak-table" "\
Setup emacspeak table browsing keys in w3 mode

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3-display-stylesheet w3-handle-style) "w3-style"
;;;;;;  "w3-style.el" (19848 47717))
;;; Generated autoloads from w3-style.el

(autoload 'w3-handle-style "w3-style" "\
Not documented

\(fn &optional PLIST)" nil nil)

(autoload 'w3-display-stylesheet "w3-style" "\
Display the stylesheet for the current document.

\(fn &optional SHEET)" t nil)

;;;***

;;;### (autoloads nil nil ("css.el" "devices.el" "images.el" "ssl.el"
;;;;;;  "url-hotlist.el" "vmsloadup.el" "w3-auto.el" "w3-cfg.el"
;;;;;;  "w3-compat.el" "w3-cus.el" "w3-emacs.el" "w3-emulate.el"
;;;;;;  "w3-fast-parse.el" "w3-imap.el" "w3-java.el" "w3-keymap.el"
;;;;;;  "w3-menu.el" "w3-mouse.el" "w3-parse.el" "w3-pkg.el" "w3-props.el"
;;;;;;  "w3-speak.el" "w3-toolbar.el" "w3-vars.el" "w3-widget.el"
;;;;;;  "w3-xemac.el") (19848 47717 752943))

;;;***

(provide 'w3-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; w3-autoloads.el ends here
