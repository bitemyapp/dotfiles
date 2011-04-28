
;;;### (autoloads (x-font-build-cache font-default-size-for-device font-default-encoding-for-device font-default-registry-for-device font-default-family-for-device font-default-object-for-device font-default-font-for-device font-create-object) "font" "lisp/font.el")

(autoload 'font-create-object "font" nil nil nil)

(autoload 'font-default-font-for-device "font" nil nil nil)

(autoload 'font-default-object-for-device "font" nil nil nil)

(autoload 'font-default-family-for-device "font" nil nil nil)

(autoload 'font-default-registry-for-device "font" nil nil nil)

(autoload 'font-default-encoding-for-device "font" nil nil nil)

(autoload 'font-default-size-for-device "font" nil nil nil)

(autoload 'x-font-build-cache "font" nil nil nil)

;;;***

;;;### (autoloads (turn-on-w3-dired w3-dired-find-file-dired w3-dired-minor-mode) "w3-dired" "lisp/w3-dired.el")

(autoload 'w3-dired-minor-mode "w3-dired" "\
Minor mode for directory browsing with Emacs-W3." t nil)

(autoload 'w3-dired-find-file-dired "w3-dired" "\
\"Edit\" directory DIR, but with additional URL-friendly bindings." t nil)

(autoload 'turn-on-w3-dired "w3-dired" "\
Unconditionally turn on W3 Dired bindings." nil nil)

;;;***

;;;### (autoloads (w3-region) "w3-display" "lisp/w3-display.el")

(autoload 'w3-region "w3-display" "\
Parse and display the region of this buffer between ST and ND." t nil)

;;;***

;;;### (autoloads (w3-do-text-entry w3-form-resurrect-widgets w3-form-add-element) "w3-forms" "lisp/w3-forms.el")

(autoload 'w3-form-add-element "w3-forms" nil nil nil)

(autoload 'w3-form-resurrect-widgets "w3-forms" nil nil nil)

(autoload 'w3-do-text-entry "w3-forms" nil nil nil)

;;;***

;;;### (autoloads (w3-hotlist-view w3-hotlist-apropos w3-hotlist-refresh w3-hotlist-delete w3-hotlist-add-document w3-hotlist-add-document-at-point w3-use-hotlist w3-parse-hotlist w3-read-html-bookmarks) "w3-hot" "lisp/w3-hot.el")

(autoload 'w3-read-html-bookmarks "w3-hot" "\
Import an HTML file into the Emacs-w3 format." t nil)

(autoload 'w3-parse-hotlist "w3-hot" "\
Read in the hotlist specified by FNAME" nil nil)

(autoload 'w3-use-hotlist "w3-hot" "\
Possibly go to a link in your W3/Mosaic hotlist.
This is part of the emacs World Wide Web browser.  It will prompt for
one of the items in your 'hotlist'.  A hotlist is a list of often
visited or interesting items you have found on the World Wide Web." t nil)

(autoload 'w3-hotlist-add-document-at-point "w3-hot" "\
Add the document pointed to by the hyperlink under point to the hotlist." t nil)

(autoload 'w3-hotlist-add-document "w3-hot" "\
Add this documents url to the hotlist" t nil)

(autoload 'w3-hotlist-delete "w3-hot" "\
Deletes a document from your hotlist file" t nil)

(autoload 'w3-hotlist-refresh "w3-hot" "\
Reload the default hotlist file into memory" t nil)

(autoload 'w3-hotlist-apropos "w3-hot" "\
Show hotlist entries matching REGEXP." t nil)

(autoload 'w3-hotlist-view "w3-hot" "\
Show the hotlist." t nil)

;;;***

;;;### (autoloads (w3-hotindex-query w3-hotindex-delete-entry w3-hotindex-rename-entry w3-hotindex-rm-key w3-hotindex-add-key) "w3-hotindex" "lisp/w3-hotindex.el")

(autoload 'w3-hotindex-add-key "w3-hotindex" "\
*Add a keyword to an item in w3-hotindex. Completion is done
on the list of all keywords." t nil)

(autoload 'w3-hotindex-rm-key "w3-hotindex" "\
*Remove a keyword from an item of w3-hotindex." t nil)

(autoload 'w3-hotindex-rename-entry "w3-hotindex" "\
Renames an entry in the HotIndex. Intended to be called from 
w3-hotlist-rename-entry. OLD should equal the entry to be renamed.
Case is therefore important." nil nil)

(autoload 'w3-hotindex-delete-entry "w3-hotindex" "\
Deletes an entry in the HotIndex. Intended to be called from 
w3-hotlist-delete. OLD should equal the entry to be deleted.
Case is therefore important." nil nil)

(autoload 'w3-hotindex-query "w3-hotindex" "\
Query the HotIndex for KEY." t nil)

;;;***

;;;### (autoloads (w3-show-dvi w3-parse-tree-to-latex) "w3-latex" "lisp/w3-latex.el")

(autoload 'w3-parse-tree-to-latex "w3-latex" nil nil nil)

(autoload 'w3-show-dvi "w3-latex" "\
Uses xdvi to show DVI file created from `w3-parse-tree-to-latex'." t nil)

;;;***

;;;### (autoloads (w3-print-this-url) "w3-print" "lisp/w3-print.el")

(autoload 'w3-print-this-url "w3-print" "\
Print out the current document" t nil)

;;;***

;;;### (autoloads (w3-table-setup-keys w3-table-speak-current-table-column) "w3-speak-table" "lisp/w3-speak-table.el")

(autoload 'w3-table-speak-current-table-column "w3-speak-table" "\
Speak current table column. Prefix arg can be used to specify the desired table nesting." t nil)

(autoload 'w3-table-setup-keys "w3-speak-table" "\
Setup emacspeak table browsing keys in w3 mode" nil nil)

;;;***

;;;### (autoloads (w3-display-stylesheet w3-handle-style) "w3-style" "lisp/w3-style.el")

(autoload 'w3-handle-style "w3-style" nil nil nil)

(autoload 'w3-display-stylesheet "w3-style" "\
Display the stylesheet for the current document." t nil)

;;;***

;;;### (autoloads (w3-prev-document w3-next-document w3-follow-link w3-follow-link-other-frame w3-do-setup w3 w3-version w3-preview-this-buffer w3-follow-url-at-point w3-follow-url-at-point-other-frame w3-maybe-follow-link w3-maybe-follow-link-mouse w3-fetch w3-fetch-other-frame w3-find-file w3-open-local) "w3" "lisp/w3.el")

(autoload 'w3-open-local "w3" "\
Find a local file, and interpret it as a hypertext document.
Prompt for an existing file or directory, and retrieve it as a
hypertext document." t nil)

(autoload 'w3-find-file "w3" "\
Find a local file, and interpret it as a hypertext document.
Prompt for an existing file or directory, and retrieve it as a
hypertext document." t nil)

(autoload 'w3-fetch-other-frame "w3" "\
Attempt to follow the hypertext reference under point in a new frame." t nil)

(autoload 'w3-fetch "w3" "\
Retrieve a document over the World Wide Web.
Defaults to URL of the current document, if any.
With prefix argument, use the URL of the hyperlink under point instead." t nil)

(autoload 'w3-maybe-follow-link-mouse "w3" "\
Maybe follow a hypertext link under point.
If there is no link under point, this will try using
`url-get-url-at-point'" t nil)

(autoload 'w3-maybe-follow-link "w3" "\
Maybe follow a hypertext link under point.
If there is no link under point, this will try using
`url-get-url-at-point'" t nil)

(autoload 'w3-follow-url-at-point-other-frame "w3" "\
Follow the URL under PT, defaults to link under (point)." t nil)

(autoload 'w3-follow-url-at-point "w3" "\
Follow the URL under PT, defaults to link under (point)." t nil)

(autoload 'w3-preview-this-buffer "w3" "\
See what this buffer will look like when its formatted as HTML.
HTML is the HyperText Markup Language used by the World Wide Web to
specify formatting for text.  More information on HTML can be found at
ftp.w3.org:/pub/www/doc." t nil)

(autoload 'w3-version "w3" "\
Show the version number of W3 in the minibuffer.
If optional argument HERE is non-nil, insert info at point." t nil)

(autoload 'w3 "w3" "\
Retrieve the default World Wide Web home page.
The World Wide Web is a global hypertext system started by CERN in
Switzerland in 1991.

The home page is specified by the variable `w3-default-homepage'.  The
document should be specified by its fully specified Uniform Resource
Locator.  The document will be parsed as HTML (if appropriate) and
displayed in a new buffer." t nil)

(autoload 'w3-do-setup "w3" "\
Do setup.
This is to avoid conflict with user settings when W3 is dumped with
Emacs." nil nil)

(autoload 'w3-follow-link-other-frame "w3" "\
Attempt to follow the hypertext reference under point in a new frame.
With prefix-arg P, ignore viewers and dump the link straight
to disk." nil nil)

(autoload 'w3-follow-link "w3" "\
Attempt to follow the hypertext reference under point.
With prefix-arg P, ignore viewers and dump the link straight
to disk." t nil)

(autoload 'w3-next-document "w3" nil t nil)

(autoload 'w3-prev-document "w3" nil t nil)

;;;***

(provide 'w3-auto)
