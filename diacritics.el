;;; diacritics.el --- Quickly insert special characters -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Enrico Flor

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/diacritics
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.4"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This packages provides a minor mode that detects long key presses
;; of certain self insertion and activates a particular transient
;; keymap, according to the specification in the variable
;; 'diacritics-alist'.  This feature is inspired by a similar one that
;; is available on Mac OS.

;;; Code:

(require 'cl-lib)

(defvar diacritics-alist '((?e . (?è ?é ?ê ?ë ?ē ?ė ?ę))
                           (?y . (?ÿ))
                           (?u . (?û ?ü ?ù ?ú ?ū))
                           (?i . (?î ?ï ?í ?ī ?į ?ì))
                           (?o . (?ô ?ö ?ò ?ó ?œ ?ø ?ō ?õ))
                           (?a . (?à ?á ?â ?ä ?æ ?ã ?å ?ā))
                           (?s . (?ß ?ś ?š))
                           (?l . (?ł))
                           (?z . (?ž ?ź ?ż))
                           (?c . (?ç ?ć ?č))
                           (?n . (?ñ ?ń))
                           (?E . (?È ?É ?Ê ?Ë ?Ē ?Ė ?Ę))
                           (?Y . (?Ÿ))
                           (?U . (?Û ?Ü ?Ù ?Ú ?Ū))
                           (?I . (?Î ?Ï ?Í ?Ī ?Į ?Ì))
                           (?O . (?Ô ?Ö ?Ò ?Ó ?Œ ?Ø ?Ō ?Õ))
                           (?A . (?À ?Á ?Â ?Ä ?Æ ?Ã ?Å ?Ā))
                           (?S . (?Ś ?Š))
                           (?L . (?Ł))
                           (?Z . (?Ž ?Ź ?Ż))
                           (?C . (?Ç ?Ć ?Č))
                           (?N . (?Ñ ?Ń)))
  "Alist associating prefix characters with a list of suffixes.
Modify this alist according to your needs, as long as all items
are formatted as characters.  For any list of suffixes, all
the elements after the first nine are going to be ignored.")




(defvar diacritics--timing 0.05
  "Maximum interval in seconds between keystrokes to count a long.
This value should be small enough to avoid havig two separate and
very fast keystrokes to be recognized as a log key press.  There
shouldn't be a reason to alter the default value of 0.05 seconds,
unless you notice that the the minor mode doesn't work as
expected, which might be due to the way the system registers
keypresses, and this value might have to be lowered or
increased.")

(defun diacritics--insert ()
  "Detect long keypresses and set a transient keymap.
This function is supposed to be evaluated at
'post-self-insert-hook'.  After each insertion of a character, if
the next insertion happens in less than the number of seconds
specified as 'diacritics--timing', and the next insertion is of
the same character as the one inserted just before it, understand
this as being a long key press.  Then, if that character is a key
in 'diacritics-alist', define a transient keymap that associates
numbers to characters, as specified in 'diacritics-alist'.
Display the mapping in the echo area."
  (when (member (preceding-char)
                (mapcar #'car
                        diacritics-alist))
    (let ((in-char (preceding-char))
          (next (read-char nil t
                           diacritics--timing)))
      (when (eq in-char next)
        (delete-char -1)
        (sleep-for 0.2)
        (discard-input)
        (let* ((keys-list
                (cdr (assq in-char
                           diacritics-alist)))
               (keys-alist
                (let ((short-list (if (> (length keys-list) 9)
                                      (cl-subseq keys-list 0 9)
                                    keys-list)))
                  (cl-mapcar (lambda (x y) (cons x y))
                             (number-sequence 1 (length short-list))
                             short-list)))
               (prompt
                (mapconcat (lambda (x) (concat (number-to-string (car x))
                                               " "
                                               (char-to-string (cdr x))))
                           keys-alist "  -  "))
               (functions
                (mapcar (lambda (x) (cons (number-to-string (car x))
                                          `(lambda ()
                                             (interactive)
                                             (insert ,(cdr x)))))
                        keys-alist))
               (keymap (make-sparse-keymap)))
          (set-transient-map keymap
                             nil
                             (message nil))
          (dolist (v functions)
            (define-key keymap (car v) (cdr v)))
          (delete-char -1)
          (message prompt))))))

;;;###autoload
(define-minor-mode diacritics-mode
  "Quickly insert special characters, input-method-independently."
  :init-value nil
  :global t
  :lighter " dia"
  :group 'convenience
  (if diacritics-mode
      (add-hook 'post-self-insert-hook 'diacritics--insert)
    (remove-hook 'post-self-insert-hook 'diacritics--insert)))

(provide 'diacritics)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; diacritics.el ends here
