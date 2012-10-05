;;; ensime-builder.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


(defun ensime-builder-build ()
  "Start the incremental builder. This command will trigger
a full recompile of the entire project!"
  (interactive)
  (setf (ensime-builder-changed-files (ensime-connection)) nil)
  (ensime-rpc-async-builder-init 'ensime-show-compile-result-buffer))

(defun ensime-builder-track-changed-files ()
  "Invoked when an ENSIME source buffer is saved. Store the filename
with all others that have been saved(modified) since the last rebuild."
  (when (and (ensime-connected-p) ;; don't want an error in a hook
	     buffer-file-name
	     (file-exists-p buffer-file-name))
    (let ((changed-files (ensime-builder-changed-files
			  (ensime-connection))))
      (when (not (memq buffer-file-name changed-files))
	(push buffer-file-name (ensime-builder-changed-files
				(ensime-connection)))))))

(defun ensime-builder-rebuild ()
  "Send a request for rebuild to the ENSIME server. Sends filenames of
all files that have been changed since the last rebuild, so incremental
builder can avoid extra work."
  (interactive)
  (let ((change-set (ensime-builder-changed-files
		     (ensime-connection))))
    (if change-set
	(progn
	  (ensime-rpc-async-builder-update
	   change-set
	   'ensime-show-compile-result-buffer)
	  (setf (ensime-builder-changed-files (ensime-connection)) nil))
      (message "Nothing to rebuild."))))

(provide 'ensime-builder)