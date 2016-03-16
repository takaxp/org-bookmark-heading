;;; org-bookmark-heading.el --- Emacs bookmark support for org-mode

;; Author: Adam Porter <adam@alphapapa.net>

;;; Installation:

;; Add to your init file:
;;
;; (require 'org-bookmark-heading)

;;; Usage:

;; Use the standard Emacs bookmark commands, "C-x r m", etc.

;;; Commentary:

;; This package provides Emacs bookmark support for org-mode.  You can
;; bookmark headings in org-mode files and jump to them using standard
;; Emacs bookmark commands.

;; It seems like this file should be named org-bookmark.el, but a
;; package by that name already exists which lets org-mode links point
;; to Emacs bookmarks, sort-of the reverse of this package.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'org)

(defun org-set-bookmark-make-record-function ()
  "Set `bookmark-make-record-function' to
`org-bookmark-make-record' in current buffer.  Should be added to
`org-mode-hook'."
  (setq-local bookmark-make-record-function 'org-bookmark-make-record))

(add-hook 'org-mode-hook 'org-set-bookmark-make-record-function)

(defun org-replace-links-in-string-with-desc (string)
  "Replace `org-mode' links in STRING with their descriptions."
  (if (string-match org-bracket-link-regexp string)
      (replace-regexp-in-string org-bracket-link-regexp
                                (lambda (text) (match-string-no-properties 3 text))
                                string)
    ;; No links found; return original string
    string))

(defun org-bookmark-make-record ()
  "Return alist for `bookmark-set' for current `org-mode'
heading.  Set org-id for heading if necessary."
  (let* ((filename (buffer-file-name))
         (org-filename (file-name-nondirectory filename))
         (heading (org-replace-links-in-string-with-desc (nth 4 (org-heading-components))))
         (name (concat org-filename ":" heading) )
         front-context-string handler)
    (when (not (and (boundp 'bookmark-name)
                    (string= bookmark-name (plist-get org-bookmark-names-plist :last-capture))))
      ;; When `org-capture-mode' is active, do not create an org-id
      ;; for the current heading.  This is because org-capture sets a
      ;; bookmark for the last capture when `org-capture-bookmark' is
      ;; non-nil, and we don't want every heading captured to get an
      ;; org-id set by this function. `bookmark-name' is set in
      ;; `org-capture-bookmark-last-stored-position' and seems to be
      ;; the way to detect whether this is being called from a capture.
      (setq front-context-string (org-id-get (point) t))
      (setq handler 'org-bookmark-jump))
    (rassq-delete-all nil `(,name
                            (filename . ,filename)
                            (handler . ,handler)
                            (front-context-string . ,front-context-string)))))

(defun org-bookmark-jump (bookmark)
  "Jump to BOOKMARK, where BOOKMARK is one whose
`front-context-string' is an org-id."
  (let ((filename (cdr (assoc 'filename bookmark)))
        (id (cdr (assoc 'front-context-string bookmark)))
        marker
        new-buffer)
    (or
     ;; Look in open and agenda files first. This way, if the node has
     ;; moved to another file, this might find it.
     (setq marker (org-id-find id t))

     (when (and filename
                (not (org-find-base-buffer-visiting filename))
                (file-exists-p filename))
       ;; Bookmark's file exists but is not open, nor in the
       ;; agenda. Find the file and look for the ID again.
       (setq new-buffer (find-file-noselect filename))
       (setq marker (org-id-find id t))))

    (if marker
        ;; Bookmark found
        (progn
          (org-goto-marker-or-bmk marker)
          (when (not (equal (buffer-file-name (marker-buffer marker)) filename))
            ;; TODO: Automatically update the bookmark?

            ;; Warn that the node has moved to another file
            (message "Heading has moved to another file; consider updating the bookmark.")))
      (progn
        ;; Bookmark not found
        (if new-buffer
            (progn
              ;; File found but not bookmark
              (kill-buffer new-buffer)  ; Don't leave buffer open
              (message "Bookmark for org-id %s not found in open org files, agenda files, or in %s." id filename))

          ;; File not found
          (message "Bookmark for org-id %s not found in open org files or agenda files, and file not found: %s" id filename))))))

(provide 'org-bookmark-heading)

;;; org-bookmark-heading.el ends here
