;;; trashcan.el --- A recoverable file deletion system

;; Copyright (C) 2006-2008 Davin Pearson

;; Author/Maintainer: Davin Pearson http://www.davinpearson.com
;; Keywords: Amiga MacOS Trash Can Windows Recycle Bin
;; Version: 1.12

;;; Limitation of Warranty

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The file trashcan.el contains a recoverable file deletion system
;; that behaves like the trash can or recycle bin that many operating
;; systems, present and past, show on their screens. This system
;; creates special directories known as trashcan directories to hold
;; files that can be deleted (permanently) or undeleted (restored /
;; recovered). On Unix systems there is one trashcan directory for
;; each user and the default values are $HOME/.TRASHCAN for each value
;; of $HOME. On Windows systems there are trashcan directories at the
;; following default locations: a:\TRASHCAN, b:\TRASHCAN, c:\TRASHCAN
;; etc.

;; This system changes the behaviour of the "x" key in dired mode from
;; permanently deleting files to a two stage system. If you are not in
;; a trashcan directory, then the selected files are moved into a
;; trashcan directory. If you are already in a trashcan directory, the
;; the selected files are permanently deleted. Files in a trashcan
;; directory can be restored by viewing that directory in dired mode,
;; selecting some files and executing the command M-x
;; trashcan-restore. All of the files in a trashcan directory can also
;; be permanently deleted in one hit by issuing the command M-x
;; trashcan-empty. The name "trashcan" comes from my old Amiga
;; computer which I still have fond memories of!

;;; Install Instructions:

;; See the following URL for the latest info and a tarball:

;; http://www.geocities.com/davinpearson/research/2006/mopa2e.html#trashcan

;; Extract the file in the above-mentioned tarball and put it
;; somewhere in load-path and load it by putting the following
;; command in your .emacs file:
;;
;; (require 'trashcan)

;;; Known Bugs:

;; (1) Doesn't preserve the marked files (*) in dired buffers when
;; files as moved in or out of a trashcan directory

;; (2) Windows detection function trashcan--is-a-windows-system could
;; be improved

;; (3) The name of this file trashcan.el might conflict with other
;; Lisp files

;;; Version History

;; Version 1.12 Added a error message if you try to delete a file or
;; folder with an exclamation mark in the name.

;; Version 1.11 Fixed a bug pointed out by Alex Scherbanov where
;; "^a-zA-Z:/" should be "^[a-zA-Z]:/".

;; Version 1.10 Debugged trashcan--rename-from-trash

;; Version 1.9 Added a new safe execution command
;; trashcan--safe-command and called it from
;; trashcan--rename-from-trash

;; Version 1.8 Debugged trashcan--rename-from-trash by adding and
;; calling new function trashcan--remove-exclamation

;; Version 1.7 Fixed a bug where you do the following actions:
;;
;;  1. Trashcan delete ~/foo/bar
;;  2. Trashcan delete ~/foo
;;  3. Trashcan restore ~/TRASHCAN/foo!bar
;;  4. Doesn't complain about missing folder ~/foo

;; Version 1.6 Fixed a bug running trashcan.el on Gnu Emacs on Mac-OS.
;; Thanks go to Martin Buchmann for pointing out this bug to me.

;; Version 1.5 Attempted to fix a bug when running this code in XEmacs

;; Version 1.4 Added a fallback in case dired-delete-file is
;; undefined.

;; Version 1.3 Fixed a compile warning re: assignment to global variable
;; trashcan--refresh-count

;; Version 1.2 Fixed a bug in trashcan--delete-dangerous.  Fixed
;; per-buffer-code argument flag of trashcan--walk-buffers to &rest.

;; Version 1.1 Removed calls to defadvice following a discussion with
;; Richard Stallman.  Changed trashcan--walk-buffers from a function
;; into a macro for faster execution in compiled form.  Changed
;; trashcan--delete-dangerous to use dired-delete-file rather
;; than shell-command rm -rvf
;;
;; Version 1.0 First version

;;; Code:

;;(setq dired-recursive-deletes 'top)
;;(setq dired-recursive-deletes 'always)

;;; (checkpoint "foo%s" "bar") (setq x '("foo%s" "bar"))
;;; (checkpoint "1") (setq x '("1"))
(defun checkpoint (&rest x) (message "*** checkpoint %s" (apply 'format x)))

(defun trashcan--is-a-windows-system ()
  (file-exists-p "c:/"))

(defun trashcan--is-a-unix-system ()
  (not (trashcan--is-a-windows-system)))

(defvar trashcan-dirname
  (if (trashcan--is-a-windows-system) "TRASHCAN" ".TRASHCAN")

  "This variable specifies what directory to move files into with
the \"x\" key in dired mode.  Do not add any prefix to the
directory such as \"~/\" or \"/\".

If this is a Windows system, the trashcan directories are located
at the following regexp:

       (concat \"^[a-zA-Z]:/\" (regexp-quote trashcan-dirname))

If this is a Unix system, there is one trashcan directory for
each user and are located at the following places:

                  (concat \"~/\" trashcan-dirname)

In Windows, DO NOT give this the same name as the windows
RECYCLER directory as this will confuse the hell out of Windows.

")

;;; (trashcan--split (setq file "d:/home/dlisp/trashcan.el"))
;;; (trashcan--split (setq file "/home/dlisp/trashcan.el"))
;;; (trashcan--split "d:/home/hairy-lemon/src/davinpearson/research/2006/trashcan.e1")

(defun trashcan--split (file)
  ;;
  ;; NOTE: this function gives meaningful results for both Windows and Unix
  ;;
  ;;(checkpoint "trashcan--split #1, file=%s" file)
  (setq file (expand-file-name file))
  (let (result)
    (setq result (if (string-match "^[a-zA-Z]:/" file)
                     (cons (substring file 0 3) (substring file 3))
                   (cons (expand-file-name "~/") (substring file 1))))
    ;;(checkpoint "trashcan--split #2, result=%s" result)
    result))

;;; (trashcan--encode (setq file "/home/foomatic.txt"))
;;; (trashcan--encode (setq file "d:/home/foomatic.txt"))
;;; (trashcan--encode (setq file "d:/home/dlisp"))
;;; (trashcan--encode (setq file "/home/dlisp/trashcan.el"))
;;;  "d:/home/TRASHCAN/home!dlisp!trashcan.el"
;;;
;;; (trashcan--encode (setq file "/home/dlisp/trashcan.el"))
;;; (trashcan--encode (setq file "d:/home/dlisp/trashcan.el"))
;;;  "d:/TRASHCAN/home!dlisp!trashcan.el"
;;;
;;; (trashcan--encode "d:/home/hairy-lemon/src/davinpearson/research/2006/trashcan.e1")
;;; (trashcan--split "d:/home/hairy-lemon/src/davinpearson/research/2006/trashcan.e1")
;;; (trashcan--encode (setq file "d:/workshop/test_file.txt"))
(defun trashcan--encode (file)
  ;;(debug)
  ;;(checkpoint "trashcan--encode #1 file=%s" file)
  (let* ((s (trashcan--split file))
         (d (car s))
         (f (cdr s)))
    ;;(checkpoint "trashcan--encode #2, s=%s" s)
    ;;(debug)
    (let ((i 0))
      (while (< i (length f))
        (if (eq ?/ (aref f i))
            (aset f i ?!))
        (incf i)))

    (let ((new (concat d trashcan-dirname "/" f)))
      (if (file-exists-p new)
          (let ((count  1)
                (result nil))
            (while (file-exists-p
                    (setq result (concat new "." (format "%d" count))))
              (incf count))
            result)
        new))))

;;; (trashcan--split "/home/TRASHCAN/home!dlisp!trashcan.el")
;;; (trashcan--split "d:/TRASHCAN/home!dlisp!trashcan.el")
;;; (trashcan--decode (setq file "/home/TRASHCAN/home!dlisp!trashcan.el"))
;;; (trashcan--decode (setq file "d:/TRASHCAN/home!dlisp!trashcan.el"))
;;; (trashcan--decode (setq file "d:/home/TRASHCAN/home!dlisp!trashcan.el"))
;;; (trashcan--decode (setq file "d:/home/.TRASHCAN/home!dlisp!trashcan.el"))
;;; (trashcan--decode (setq file "/Users/Martin/.TRASHCAN/Users!Martin!Desktop!test.txt"))
;;; (setq trashcan-dirname ".TRASHCAN")

(defun trashcan--decode (file)

  ;;(checkpoint "trashcan--decode 1")

  (if (string-match (concat "^[a-zA-Z]:/" (regexp-quote trashcan-dirname)) file)
      (progn
        ;;
        ;; NOTE: we are in Windows mode in this branch
        ;;
        ;;(checkpoint "trashcan--decode 2")
        (let ((d (substring file 0 3))
              (f (substring file (+ 4 (length trashcan-dirname))))
              (i 0))
          (while (< i (length f))
            (if (eq ?! (aref f i))
                (aset f i ?/))
            (incf i))
          ;;(checkpoint "trashcan--decode 3")
          (concat d f))
        )

    (let (sm)
      ;;
      ;; NOTE: we are in Unix mode in this branch
      ;;
      ;; (setq trashcan-dirname ".TRASHCAN")
      (assert (string-match (concat "^" (expand-file-name "~/") (regexp-quote trashcan-dirname)) file))
      ;;(checkpoint "trashcan--decode 4")
      (setq sm "/[^/]*$")
      ;;(checkpoint "trashcan--decode 5 sm=%s" sm)
      (assert (string-match sm file))
      ;;(checkpoint "6")
      (let ((x (substring file (match-beginning 0) (match-end 0)))
            (i 0))
        ;;(checkpoint "trashcan--decode 7 x=%s" x)
        (while (< i (length x))
          (if (eq ?! (aref x i))
              (aset x i ?/))
          (incf i))
        ;;(checkpoint "trashcan--decode 8 x=%s" x)
        x
        )))
  )

(quote
 (trashcan--walk-buffers (if (and (buffer-file-name)
                                  (string-match "\\.el$" (buffer-file-name)))
                             (beeps "found %s" (buffer-file-name))))

 )

(defmacro trashcan--walk-buffers (&rest per-buffer-code)
  ;;
  ;; NOTE: a long name trashcan--walk-buffers--ptr is used here to
  ;; guard against accidental aliasing
  ;;
  `(save-window-excursion
    (let ((trashcan--walk-buffers--ptr (buffer-list)))
      (while trashcan--walk-buffers--ptr
        (set-buffer (car trashcan--walk-buffers--ptr))
        ,(cons 'progn per-buffer-code)
        (setq trashcan--walk-buffers--ptr (cdr trashcan--walk-buffers--ptr))
        ))))

;;; (trashcan--delete-dangerous "d:/TRASHCAN/workspace/")
;;; (trashcan--delete-dangerous "c:/TRASHCAN")
;;; (setq file-or-directory "d:/TRASHCAN/")
(defun trashcan--delete-dangerous (file-or-directory)
  "Is dangerous because it can delete non-empty folders"
  (assert (file-exists-p file-or-directory))
  (if (fboundp 'dired-delete-file)
      (dired-delete-file file-or-directory (if (file-directory-p file-or-directory) 'always nil))
    ;;
    ;; NOTE: fallback in case this command does not exist
    ;;
    (shell-command (concat "rm -rvf \"" file-or-directory "\""))))

;;; (trashcan--in-windows-trashcan filename)
(defun trashcan--in-windows-trashcan (filename &optional OR-SUBDIR)
  "Returns the relevant windows trashcan
directory or nil if there isn't one"
  (setq filename (expand-file-name filename))
  (let ((dirname (file-name-directory filename)))
    (if OR-SUBDIR
        (if (string-match (concat "^\\([a-zA-Z]:/"
                                  (regexp-quote trashcan-dirname)
                                  "\\)")
                          dirname)
            (substring dirname (match-beginning 1) (match-end 1)))
      (if (string-match (concat "^\\([a-zA-Z]:/"
                                (regexp-quote trashcan-dirname)
                                "\\)/?$")
                        dirname)
          (substring dirname (match-beginning 1) (match-end 1))))))

(defun trashcan--in-unix-trashcan (filename &optional OR-SUBDIR)
  "Returns the relevant unix trashcan directory or nil if there isn't one"
  (setq filename (expand-file-name filename))
  (let ((dirname (file-name-directory filename)))
    (if OR-SUBDIR
        (if (string-match (concat "^"
                                  (expand-file-name "~/")
                                  (regexp-quote trashcan-dirname))
                          dirname)
            (concat (expand-file-name "~/") trashcan-dirname))
      (if (string-match (concat "^"
                                (expand-file-name "~/")
                                (regexp-quote trashcan-dirname)
                                "/?$")
                        dirname)
          (concat (expand-file-name "~/") trashcan-dirname)))))

(defun trashcan--in-trashcan (filename &optional OR-SUBDIR)
  (or (trashcan--in-windows-trashcan filename OR-SUBDIR)
      (trashcan--in-unix-trashcan filename OR-SUBDIR)))

(defun trashcan--after-permanent-deletion ()
  ;;
  ;; NOTE: conditionally kills file buffers that have been deleted
  ;;
  ;; NOTE: unconditionally kills dired buffers that have been deleted
  ;;
  (let (dirname)
    (cond
     ((setq dirname (trashcan--in-windows-trashcan default-directory 'OR-SUBDIR)))
     ((setq dirname (trashcan--in-unix-trashcan    default-directory 'OR-SUBDIR)))
     (t
      (error "Should never happen")))

    (trashcan--walk-buffers
     (if (or (and (buffer-file-name)
                  (string-match (concat "^" dirname)
                                default-directory)
                  (y-or-n-p (concat "Kill buffer "
                                    (buffer-file-name)
                                    " too? ")))
             (and (eq major-mode 'dired-mode)
                  (not (file-exists-p default-directory))))
         (kill-buffer nil)))))

(defvar trashcan--global-refresh-count 1)
(defvar trashcan--refresh-count nil)

;; (setq file-list '("d:/workshop/test_file.txt"))
;; (setq ptr file-list)
(defun trashcan--rename-to-trash (file-list)

  (let ((dir nil))
    (let ((ptr file-list))
      (while ptr
        ;;
        ;; NOTE: Creates a trash directory if none exists, then renames the file to trash directory.
        ;;
        (let* ((new-name (trashcan--encode (car ptr)))
               (fnd      (file-name-directory new-name)))

          ;;(checkpoint "trashcan--rename-to-trash (car ptr)=%s new-name=%s" (car ptr) new-name)

          ;;(debug)

          (make-directory fnd 'PARENTS)
          (setq dir fnd)
          (checkpoint "trashcan--rename-to-trash (car ptr)=%s new-name=%s" (car ptr) new-name)
          (rename-file (car ptr) new-name))
        (setq ptr (cdr ptr)))

      ;;(debug)

      (setq ptr file-list)

      (incf trashcan--global-refresh-count)

      (if (not (boundp 'trashcan--refresh-count))
          (setq-default trashcan--refresh-count nil))

      (while ptr

        (trashcan--walk-buffers
         (progn
           (make-local-variable 'trashcan--refresh-count)
           (if (and (buffer-file-name)
                    (string-match (concat "^" (regexp-quote (car ptr)))
                                  (buffer-file-name))
                    (not (eq trashcan--global-refresh-count trashcan--refresh-count)))
               (set-visited-file-name (trashcan--encode (car ptr)) 'NO-QUERY))
           (setq trashcan--refresh-count trashcan--global-refresh-count)))

        ;;
        ;; NOTE: reverts all direds of the original file
        ;;
        (let ((dirname (file-name-directory (car ptr))))
          (trashcan--walk-buffers
           (progn
             (make-local-variable 'trashcan--refresh-count)
             (if (and (eq major-mode 'dired-mode)
                      (string-match (concat "^"
                                            (regexp-quote dirname)
                                            "/?$") default-directory)
                      (not (eq trashcan--global-refresh-count trashcan--refresh-count)))
                 (revert-buffer))
             (set (make-local-variable 'trashcan--refresh-count)
                  trashcan--global-refresh-count))))

        (setq ptr (cdr ptr))))

    (if (trashcan--is-a-windows-system)
        (setq dir (downcase dir)))

    ;;
    ;; NOTE: deletes all dired buffers that have had their dirs deleted
    ;;
    (trashcan--walk-buffers
     (if (and (eq major-mode 'dired-mode)
              (not (file-exists-p (expand-file-name default-directory))))
         (kill-buffer nil)))

    ;;
    ;; NOTE: reverts trashcan buffers that have been changed
    ;;
    (trashcan--walk-buffers
     (if (and (eq major-mode 'dired-mode) (string=
                                           (if (trashcan--is-a-windows-system)
                                               (downcase default-directory)
                                             default-directory) dir))
         (revert-buffer)))))

(require 'dired)

;;; NOTE: This replaces the function with the same name as the one in
;;; dired.el
;;;
;;; NOTE: The purpose of &rest ignore is for compatibility with XEmacs
;;;
(defun dired-internal-do-deletions (l arg &rest ignore)

  ;;(beeps "Calling dired-internal-do-deletions")
  ;;(defun dired-internal-do-deletions (l arg)

  "This function replaces the function of the
same name in the standard Emacs file dired.el"
  ;;(d-foo)

  (if (not (eq major-mode 'dired-mode))
      (error "You must be in dired mode to execute dired-internal-do-deletions"))

  ;;(debug)

  (let ((ptr l))
    (while ptr
      ;;(debug)
      (if (or (string-match "/\\./?$" (caar ptr))
              (string-match "/\\.\\./?$" (caar ptr)))
          (error "You cannot delete the directories . or .."))
      (setq ptr (cdr ptr))))

  (let ((ptr l))
     (while ptr
       ;;(checkpoint "s=%s" (caar ptr))
       (if (string-match "!" (caar ptr)) (error "Please remove ! character fron file: %s" (caar ptr)))
       (if (or (string-match (concat "^[a-zA-Z]:/"
                                     (regexp-quote trashcan-dirname)
                                     "/?$")
                             (caar ptr))
               (string-match (concat "^"
                                     (expand-file-name "~/")
                                     (regexp-quote trashcan-dirname)
                                     "/?$")
                             (caar ptr)))

           (progn
             ;;(debug)
             (error (concat "You cannot move a trashcan directory (%s)"
                            " into a trashcan directory "
                            "(Try \"rm -r\" instead)")
                    trashcan-dirname)))
       (setq ptr (cdr ptr))))

  ;;(debug)

  (let ((in-trash (trashcan--in-trashcan default-directory 'OR-SUBDIR))
        (files (mapcar (function car) l)))

    ;; NOTE: these two have the same result...
    (setq files (nreverse (mapcar (function dired-make-relative) files)))
    ;;(setq files (nreverse (mapcar 'dired-make-relative files)))

    ;;(debug)

    (if in-trash
        (if (dired-mark-pop-up " *Deletions*"
                               'delete
                               files
                               dired-deletion-confirmer
                               (format "Permanently Delete %s "
                                       (dired-mark-prompt arg files)))
            (let ((ptr l))

              ;;(debug)
              (while ptr
                (trashcan--delete-dangerous (caar ptr))
                (message "Deleted file %s " (caar ptr))
                (setq ptr (cdr ptr)))
              ;;(debug)
              (revert-buffer)
              (trashcan--after-permanent-deletion)))

      (if (dired-mark-pop-up " *Deletions*"
                               'delete
                               files
                               dired-deletion-confirmer
                               (format "Move to trashcan %s "
                                       (dired-mark-prompt arg files)))
          (let ((ptr l)
                (list nil))
            (while ptr
              (setq list (cons (caar ptr) list))
              (setq ptr (cdr ptr)))

            ;;(debug)
            (trashcan--rename-to-trash list)
            (revert-buffer))))))

(defun trashcan--make-absolute (filename)
  (setq filename (expand-file-name filename))

  (if (string-match "/$" filename)
      (setq filename (substring filename 0 (1- (length filename)))))

  ;;(debug)

  (if (not (or (string-match "^[a-zA-Z]:/" filename)
               (string-match "^/" filename)))
      (concat (expand-file-name default-directory) filename)
    filename))

;;; (setq file "home!plotter!2008/caleb")
;;; (setq i 0)
(defun trashcan--remove-exclamation (file)
  (let ((i 0))
    (while (< i (length file))
      (if (eq (aref file i) ?!)
          (aset file i ?/))
      (incf i))
    file))


;; (trashcan--safe-command '(make-directory "d:/"))
;; (trashcan--safe-command (d-foo) (/ 0 0))
;;(defun trashcan--safe-command (cmd &optional)
;;  (condition-case err
;;      (eval cmd)
;;    (error )))

(defmacro trashcan--safe-command (&rest cmd)
  `(condition-case err
      ,(cons 'progn cmd)
     (error )))

;; (trashcan--safe-command (/ 0 0))
;; (setq ptr '("d:/TRASHCAN/foo"))
;; (setq ptr '("d:/home/TRASHCAN/foo"))
;; (car ptr)
;; (setq source "d:/TRASHCAN/foo")
;; (setq source "d:/home/TRASHCAN/foo")
;; (setq target "d:/foo")
;; (rename-file "d:/TRASHCAN/foo/intrash/" "d:/foo/intrash")
;; (rename-file "d:/abc" "d:/111/222/abc")

(defun trashcan--rename-from-trash (source target)
  ;;(debug)
  (if (file-directory-p source)
      (let* ((list (cddr (directory-files source t)))
             (ptr  list))
        (while ptr
          ;;(checkpoint "trashcan--rename-from-trash 1")
          (rename-file (car ptr) target)
          (setq ptr (cdr ptr)))
        (delete-directory source))
    ;;(checkpoint "trashcan--rename-from-trash 2")
    (rename-file source target)
    ))

(defun trashcan-restore ()
  (interactive)

  ;;(checkpoint "trashcan-restore 1")

  (if (not (trashcan--in-trashcan default-directory))
      (error (concat "You must be in the trashcan directory (%s)"
                     " to execute this command")
             trashcan-dirname))

  ;;(checkpoint "trashcan-restore 2")

  (let* ((list (dired-get-marked-files))
         (ptr  list))

    (while ptr
      (let* ((source (car ptr))
             (target (trashcan--decode source))
             (fnd    (file-name-directory target)))

        ;;(checkpoint "trashcan-restore 3 source=%s, target=%s fnd=%s" source target fnd)
        ;;(debug)

        ;;(if (file-exists-p target)
        ;;    (error "File %s already exists" target)
        (if (file-directory-p source)
            (make-directory target 'PARENTS))

        (make-directory fnd 'PARENTS)
        ;;(debug)
        ;;(rename-file source target)
        (trashcan--rename-from-trash source target)
        ;;(checkpoint "trashcan-restore 4")

        ;;
        ;; NOTE: are we editing one of the files
        ;; that we want to restore?
        ;;
        ;;(trashcan--walk-buffers
        ;; '(if (string= (buffer-file-name) source)
        ;;      (set-visited-file-name target 'NO-QUERY)))

        ;;
        ;; NOTE: are we editing a files of a subdirectory
        ;; that we want to restore
        ;;
        (trashcan--walk-buffers
         (if (and (buffer-file-name)
                  (string-match (concat "^" (regexp-quote source)) (buffer-file-name)))
             (let ((n (substring (buffer-file-name) (length source))))
               ;;(debug)
               (set-visited-file-name (concat target n) 'NO-QUERY))))

        ;;(checkpoint "trashcan-restore 5")

        (trashcan--walk-buffers
         (if (and (eq major-mode 'dired-mode)
                  (string= fnd (expand-file-name default-directory)))
             (revert-buffer)))

        ;;(checkpoint "trashcan-restore 6")

        (trashcan--walk-buffers
         (if (and (eq major-mode 'dired-mode)
                  (not (file-exists-p (expand-file-name default-directory))))
             (kill-buffer nil)))

        ;;(checkpoint "trashcan-restore 7")

        )
      (setq ptr (cdr ptr))))

  ;;(checkpoint "trashcan-restore 8")

  (trashcan--walk-buffers
   (if (and (eq major-mode 'dired-mode)
            (trashcan--in-trashcan default-directory 'OR-SUBDIR))
       (revert-buffer)))

  ;;(checkpoint "trashcan-restore 9")

  )

(defun trashcan-empty ()
  "Careful when using this command as it cannot be undone"
  (interactive)
  (cond
   ((not (trashcan--in-trashcan default-directory))
    (error "You must be in the trashcan to execute this command"))

   ((not (eq major-mode 'dired-mode))
    (error "You must be in dired mode to execute this command"))

   (t
    (if (yes-or-no-p "Really empty trashcan? ")
        (let (dirname)

          (cond
           ((setq dirname (trashcan--in-windows-trashcan default-directory)))
           ((setq dirname (trashcan--in-unix-trashcan    default-directory)))
           (t
            (error "Should never happen")))

          ;;(debug)

          (save-window-excursion
            (trashcan--delete-dangerous dirname))

          ;;(beeps "Deleting file %s" dirname)

          (make-directory dirname 'PARENTS)
          (revert-buffer)
          (trashcan--after-permanent-deletion))))))

(provide 'trashcan)
;;; trashcan.el ends here
